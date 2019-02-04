package special.sigma

import org.ergoplatform.{ErgoLikeContext, ErgoLikeTransaction, ErgoBox}
import org.ergoplatform.ErgoBox.NonMandatoryRegisterId
import scalan.{Nullable, RType}
import scorex.crypto.hash.Digest32
import sigmastate.{AvlTreeData, SType}
import sigmastate.Values.{ErgoTree, EvaluatedValue}
import sigmastate.lang.Terms.ValueOps
import sigmastate.eval.IRContext
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.{CostedProverResult, ProverResult}
import sigmastate.interpreter.Interpreter.{ScriptNameProp, ScriptEnv, emptyEnv}
import sigmastate.utxo.ErgoLikeTestInterpreter
import special.collection.Coll

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.util.Try


case class SpecContext(testSuite: SigmaTestingCommons)(implicit val IR: IRContext) {
  val dsl: SigmaDslBuilder = new TestSigmaDslBuilder
  type PropositionFunc = Context => SigmaProp
  type TokenId = Coll[Byte]
  case class ErgoScript(env: ScriptEnv, code: String)

  case class PropositionSpec(name: String, dslSpec: PropositionFunc, scriptSpec: ErgoScript) {
    lazy val ergoTree: ErgoTree = testSuite.compileWithCosting(scriptSpec.env, scriptSpec.code)
  }
  
  trait ContractSyntax { contract: SigmaContract =>
    val syntax = new ExtensionMethods(builder)

    def Coll[T](items: T*)(implicit cT: RType[T]) = builder.Colls.fromItems(items:_*)

    def proposition(name: String, dslSpec: PropositionFunc, scriptEnv: ScriptEnv, scriptCode: String) =
      PropositionSpec(name, dslSpec, ErgoScript(scriptEnv, scriptCode))

    def Env(entries: (String, Any)*): ScriptEnv = Map(entries:_*)
  }

  trait ContractSpec extends DefaultContract with ContractSyntax

  trait ProtocolParty {
    def name: String
  }

  case class ProvingParty(name: String) extends ProtocolParty {
    private val prover = new ErgoLikeTestProvingInterpreter
    val pubKey: SigmaProp = ProveDlogEvidence(prover.dlogSecrets.head.publicImage.h)

    def prove(inBox: InBox): Try[CostedProverResult] = {
      val boxToSpend = inBox.utxoBox
      val propSpec: PropositionSpec = boxToSpend.propSpec
      val ctx = inBox.toErgoContext
      val env = propSpec.scriptSpec.env + (ScriptNameProp -> (propSpec.name + "_prove"))
      val prop = propSpec.ergoTree.proposition.asBoolValue
      val proof = prover.prove(env, prop, ctx, testSuite.fakeMessage)
      proof
    }
  }

  case class VerifyingParty(name: String) extends ProtocolParty {
    private val verifier = new ErgoLikeTestInterpreter

    def verify(inBox: InBox, proverResult: ProverResult) = {
      val boxToSpend = inBox.utxoBox
      val propSpec = boxToSpend.propSpec
      val ctx = inBox.toErgoContext
      val env = propSpec.scriptSpec.env + (ScriptNameProp -> (propSpec.name + "_verify"))
      val prop = propSpec.ergoTree.proposition.asBoolValue
      verifier.verify(env, prop, ctx, proverResult, testSuite.fakeMessage).get._1
    }
  }

  case class InBox(tx: Transaction, utxoBox: OutBox) {
    def toErgoContext: ErgoLikeContext = {
      val propSpec: PropositionSpec = utxoBox.propSpec
      val ctx = ErgoLikeContext(
        currentHeight = tx.block.height,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContext.dummyPubkey,
        boxesToSpend = tx.inputs.map(_.utxoBox.ergoBox).toIndexedSeq,
        spendingTransaction = ErgoLikeTransaction(IndexedSeq(), tx.outputs.map(_.ergoBox).toIndexedSeq),
        self = utxoBox.ergoBox)
      ctx
    }
  }

  case class OutBox(tx: Transaction, boxIndex: Int, value: Long, propSpec: PropositionSpec) {
    private var _tokens: Seq[(TokenId, Long)] = Seq()
    private var _regs: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]] = Map()

    def withTokens(tokens: (TokenId, Long)*) = { _tokens = tokens.toSeq; this }
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

    lazy val ergoBox = {
      val tokens = _tokens.map { case (id, v) => (Digest32 @@ id.toArray, v) }
      ErgoBox(value, propSpec.ergoTree, tx.block.height, tokens, _regs)
    }
    def id = ergoBox.id
  }

  case class Transaction(block: Block)  {
    private val _inputs: ArrayBuffer[InBox] = mutable.ArrayBuffer.empty[InBox]
    def inputs: Seq[InBox] = _inputs

    private val _outputs = mutable.ArrayBuffer.empty[OutBox]
    def outputs: Seq[OutBox] = _outputs

    def inBox(utxoBox: OutBox) = {
      val box = InBox(this, utxoBox)
      _inputs += box
      box
    }

    def outBox(value: Long, propSpec: PropositionSpec) = {
      val box = OutBox(this, _outputs.size, value, propSpec)
      _outputs += box
      box
    }

    def spending(utxos: OutBox*) = {
      for (b <- utxos) inBox(b)
      this
    }


  }

  case class Block(height: Int) {

    def transaction() = Transaction(this)

  }

  def block(height: Int) = Block(height)

  implicit def Coll[T](items: Array[T])(implicit cT: RType[T]) = dsl.Colls.fromArray(items)

}



