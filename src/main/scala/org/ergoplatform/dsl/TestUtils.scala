package org.ergoplatform.dsl

import org.ergoplatform.{ErgoLikeContext, ErgoBox}
import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, BoxId}
import scalan.RType
import sigmastate.SType
import sigmastate.SType.AnyOps
import org.ergoplatform.dsl.ContractSyntax.{Token, TokenId, ErgoScript, Proposition}
import sigmastate.Values.{ErgoTree, Constant}
import sigmastate.eval.{IRContext, CSigmaProp, CostingSigmaDslBuilder, Evaluation}
import sigmastate.interpreter.{ProverResult, CostedProverResult}
import sigmastate.interpreter.Interpreter.ScriptEnv
import special.collection.Coll
import special.sigma.{SigmaProp, SigmaContract, AnyValue, Context, DslSyntaxExtensions, SigmaDslBuilder}

import scala.language.implicitConversions
import scala.util.Try

trait ContractSyntax { contract: SigmaContract =>
  override def builder: SigmaDslBuilder = new CostingSigmaDslBuilder
  val spec: ContractSpec
  val syntax = new DslSyntaxExtensions(builder)
  def contractEnv: ScriptEnv

  /** The default verifier which represents miner's role in verification of transactions.
    * It can be overriden in derived classes. */
  lazy val verifier: spec.VerifyingParty = spec.VerifyingParty("Miner")

  def Coll[T](items: T*)(implicit cT: RType[T]) = builder.Colls.fromItems(items:_*)

  def proposition(name: String, dslSpec: Proposition, scriptCode: String) = {
    val env = contractEnv.mapValues { v =>
      val tV = Evaluation.rtypeOf(v).get
      val treeType = Evaluation.toErgoTreeType(tV)
      val data = Evaluation.fromDslData(v, treeType)(spec.IR)
      val elemTpe = Evaluation.rtypeToSType(treeType)
      spec.IR.builder.mkConstant[SType](data.asWrappedType, elemTpe)
    }
    spec.mkPropositionSpec(name, dslSpec, ErgoScript(env, scriptCode))
  }

  def Env(entries: (String, Any)*): ScriptEnv = Map(entries:_*)
}
object ContractSyntax {
  type Proposition = Context => SigmaProp
  type TokenId = Coll[Byte]
  case class ErgoScript(env: ScriptEnv, code: String)
  case class Token(id: TokenId, value: Long)
}

trait SigmaContractSyntax extends SigmaContract with ContractSyntax {
  override def canOpen(ctx: Context): Boolean = ???
}

trait ContractSpec {
  val dsl: SigmaDslBuilder = CostingSigmaDslBuilder
  val Colls = dsl.Colls

  implicit def Coll[T](items: Array[T])(implicit cT: RType[T]) = Colls.fromArray(items)

  val IR: IRContext

  import SType.AnyOps
  implicit class DslDataOps[A](data: A)(implicit tA: RType[A]) {
    def toTreeData: Constant[SType] = {
      val treeType = Evaluation.toErgoTreeType(tA)
      val treeData = Evaluation.fromDslData(data, tRes = treeType)(IR)
      IR.builder.mkConstant(treeData.asWrappedType, Evaluation.rtypeToSType(tA))
    }
  }

  trait PropositionSpec {
    def name: String
    def dslSpec: Proposition
    def scriptSpec: ErgoScript
    def ergoTree: ErgoTree
  }
  object PropositionSpec {
    def apply(name: String, dslSpec: Proposition, scriptSpec: ErgoScript) = mkPropositionSpec(name, dslSpec, scriptSpec)
  }

  private[dsl] def mkPropositionSpec(name: String, dslSpec: Proposition, scriptSpec: ErgoScript): PropositionSpec


  trait ProtocolParty {
    def name: String
  }

  /** Represents a participant of blockchain scenario (protocol). Participants are identified by `pubKey`
    * and may have human readable names.
    * This type of participant can generate proof for input boxes. */
  trait ProvingParty extends ProtocolParty {
    /** Public key of this party represented as sigma protocol proposition.
      * Thus, it can be used in logical `&&`, `||` and `atLeast` propositions.
      * For example `(HEIGHT > 10 && bob.pubKey) || (HEIGHT <= 10 && alice.pubKey). */
    def pubKey: SigmaProp

    /** Generate proof for the given `inBox`. The input box has attached guarding proposition,
      * which is executed in the Context, specifically created for `inBox`.*/
    def prove(inBox: InputBox, extensions: Map[Byte, AnyValue] = Map()): Try[CostedProverResult]
  }
  object ProvingParty {
    def apply(name: String): ProvingParty = mkProvingParty(name)
  }
  protected def mkProvingParty(name: String): ProvingParty

  trait VerifyingParty extends ProtocolParty {
    /** Verifies the proof generated by the ProvingParty (using `prove` method) for the given `inBox`.*/
    def verify(inBox: InputBox, proverResult: ProverResult): Boolean
  }
  object VerifyingParty {
    def apply(name: String): VerifyingParty = mkVerifyingParty(name)
  }
  protected def mkVerifyingParty(name: String): VerifyingParty

  trait InputBox {
    def tx: Transaction
    def utxoBox: OutBox
    def runDsl(extensions: Map[Byte, AnyValue] = Map()): SigmaProp
    private [dsl] def toErgoContext: ErgoLikeContext
  }

  trait OutBox {
    def id: BoxId
    def tx: Transaction
    def boxIndex: Int
    def value: Long
    def propSpec: PropositionSpec
    def withTokens(tokens: Token*): OutBox
    def withRegs(regs: (NonMandatoryRegisterId, Any)*): OutBox
    def token(id: TokenId): Token
    private[dsl] def ergoBox: ErgoBox
  }

  trait Transaction extends ChainTransaction {
    def block: Block
    def inputs: Seq[InputBox]
    def outputs: Seq[OutBox]
    def inBox(utxoBox: OutBox): InputBox
    def outBox(value: Long, propSpec: PropositionSpec): OutBox
    def spending(utxos: OutBox*): Transaction
  }

  trait ChainTransaction {
    def outputs: Seq[OutBox]
  }

  /** Block which is already in blockchain. */
  trait ChainBlock {
    def getTransactions(): Seq[ChainTransaction]
  }

  /** Block which serve as transaction context. */
  trait Block extends ChainBlock {
    def height: Int
    def newTransaction(): Transaction
  }

  val MinErgValue = 1
  def error(msg: String) = sys.error(msg)

}




