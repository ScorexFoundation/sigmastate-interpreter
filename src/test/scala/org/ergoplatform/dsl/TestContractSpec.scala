package org.ergoplatform.dsl

import scala.util.Try
import sigmastate.interpreter.Interpreter.ScriptNameProp
import special.sigma.{AnyValue, SigmaProp, TestValue}
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeTestInterpreter, SigmaTestingCommons}
import scorex.crypto.hash.Digest32
import org.ergoplatform.ErgoBox.NonMandatoryRegisterId
import sigmastate.lang.Terms.ValueOps

import scala.collection.mutable
import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeTransaction}
import sigmastate.Values.{ErgoTree, EvaluatedValue}
import sigmastate.{AvlTreeData, SType}

import scala.collection.mutable.ArrayBuffer
import scalan.Nullable
import sigmastate.interpreter.{CostedProverResult, ProverResult}
import sigmastate.eval.{CSigmaProp, Evaluation, IRContext}
import org.ergoplatform.dsl.ContractSyntax.{ErgoScript, Proposition, Token, TokenId}

case class TestContractSpec(testSuite: SigmaTestingCommons)(implicit val IR: IRContext) extends ContractSpec {

  case class TestPropositionSpec(name: String, dslSpec: Proposition, scriptSpec: ErgoScript) extends PropositionSpec {
    lazy val ergoTree: ErgoTree = {
      val value = testSuite.compileWithCosting(scriptSpec.env, scriptSpec.code)
      val tree: ErgoTree = value.asSigmaProp
      tree
    }
  }

  override private[dsl] def mkPropositionSpec(name: String, dslSpec: Proposition, scriptSpec: ErgoScript) =
    TestPropositionSpec(name, dslSpec, scriptSpec)


  case class TestProvingParty(name: String) extends ProvingParty {
    private val prover = new ContextEnrichingTestProvingInterpreter

    val pubKey: SigmaProp = CSigmaProp(prover.dlogSecrets.head.publicImage)

    import SType.AnyOps
    def prove(inBox: InputBox, extensions: Map[Byte, AnyValue] = Map()): Try[CostedProverResult] = {
      val boxToSpend = inBox.utxoBox
      val propSpec: PropositionSpec = boxToSpend.propSpec
      val bindings = extensions.mapValues { case v: TestValue[a] =>
        implicit val tA = v.tA
        val treeType = Evaluation.toErgoTreeType(tA)
        val treeData = Evaluation.fromDslData(v.value, tRes = treeType)
        IR.builder.mkConstant(treeData.asWrappedType, Evaluation.rtypeToSType(v.tA))
      }
      val ctx = inBox.toErgoContext
//      val newExtension = ContextExtension(ctx.extension.values ++ bindings)
      val env = propSpec.scriptSpec.env + (ScriptNameProp -> (propSpec.name + "_prove"))
      val prop = propSpec.ergoTree
      val p = bindings.foldLeft(prover) { (p, b) => p.withContextExtender(b._1, b._2) }
      val proof = p.prove(env, prop, ctx, testSuite.fakeMessage)
      proof
    }
  }

  override protected def mkProvingParty(name: String): ProvingParty = TestProvingParty(name)

  case class TestVerifyingParty(name: String) extends VerifyingParty {
    private val verifier = new ErgoLikeTestInterpreter

    def verify(inBox: InputBox, proverResult: ProverResult) = {
      val boxToSpend = inBox.utxoBox
      val propSpec = boxToSpend.propSpec
      val ctx = inBox.toErgoContext
      val env = propSpec.scriptSpec.env + (ScriptNameProp -> (propSpec.name + "_verify"))
      val prop = propSpec.ergoTree
      verifier.verify(env, prop, ctx, proverResult, testSuite.fakeMessage).get._1
    }
  }

  override protected def mkVerifyingParty(name: String): VerifyingParty = TestVerifyingParty(name)

  case class TestInputBox(tx: Transaction, utxoBox: OutBox) extends InputBox {
    private [dsl] def toErgoContext: ErgoLikeContext = {
      val propSpec = utxoBox.propSpec
      val ctx = ErgoLikeContext(
        currentHeight = tx.block.height,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContext.dummyPubkey,
        boxesToSpend = tx.inputs.map(_.utxoBox.ergoBox).toIndexedSeq,
        spendingTransaction = ErgoLikeTransaction(IndexedSeq(), tx.outputs.map(_.ergoBox).toIndexedSeq),
        self = utxoBox.ergoBox)
      ctx
    }
    def runDsl(extensions: Map[Byte, AnyValue] = Map()): SigmaProp = {
      val ctx = toErgoContext.toSigmaContext(IR, false, extensions)
      val res = utxoBox.propSpec.dslSpec(ctx)
      res
    }
  }

  case class TestOutBox(tx: Transaction, boxIndex: Int, value: Long, propSpec: PropositionSpec) extends OutBox {
    private var _tokens: Seq[Token] = Seq()
    private var _regs: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]] = Map()

    def withTokens(tokens: Token*) = { _tokens = tokens.toSeq; this }
    def withRegs(regs: (NonMandatoryRegisterId, Any)*) = {
      _regs = regs.map { case (id, v) =>
        val lifted = IR.builder.liftAny(v) match {
          case Nullable(v) => v
          case _ =>
            sys.error(s"Invalid value for register R${id.number}: $v")
        }
        (id, lifted.asInstanceOf[EvaluatedValue[_ <: SType]])
      }.toMap
      this
    }

    override def token(id: TokenId): Token = {
      val optToken = _tokens.collectFirst { case t if t.id == id => t }
      optToken.getOrElse(sys.error(s"Token with id=$id not found in the box $this"))
    }

    private[dsl] lazy val ergoBox: ErgoBox = {
      val tokens = _tokens.map { t => (Digest32 @@ t.id.toArray, t.value) }
      ErgoBox(value, propSpec.ergoTree, tx.block.height, tokens, _regs)
    }
    def id = ergoBox.id
  }

  case class TestTransaction(block: Block) extends Transaction {
    private val _inputs: ArrayBuffer[InputBox] = mutable.ArrayBuffer.empty[InputBox]
    def inputs: Seq[InputBox] = _inputs

    private val _outputs = mutable.ArrayBuffer.empty[OutBox]
    def outputs: Seq[OutBox] = _outputs

    def inBox(utxoBox: OutBox) = {
      val box = TestInputBox(this, utxoBox)
      _inputs += box
      box
    }

    def outBox(value: Long, propSpec: PropositionSpec) = {
      val box = TestOutBox(this, _outputs.size, value, propSpec)
      _outputs += box
      box
    }

    def spending(utxos: OutBox*) = {
      for (b <- utxos) inBox(b)
      this
    }

  }

  case class TestBlock(height: Int) extends Block {
    def newTransaction() = TestTransaction(this)
    override def getTransactions(): Seq[ChainTransaction] = ???
  }

  def block(height: Int): Block = TestBlock(height)
}
