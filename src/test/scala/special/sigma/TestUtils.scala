package special.sigma

import org.ergoplatform.ErgoBox
import scalan.RType
import sigmastate.Values.ErgoTree
import sigmastate.eval.IRContext
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.utxo.ErgoLikeTestInterpreter
import scala.language.implicitConversions

trait ContractSyntax { contract: SigmaContract =>
  val specContext: SpecContext
  import specContext._

  val syntax = new ExtensionMethods(builder)

  def Coll[T](items: T*)(implicit cT: RType[T]) = builder.Colls.fromItems(items:_*)

  def proposition(name: String, dslSpec: PropositionFunc, scriptEnv: ScriptEnv, scriptCode: String) =
    PropositionSpec(name, dslSpec, ErgoScript(scriptEnv, scriptCode))

  def Env(entries: (String, Any)*): ScriptEnv = Map(entries:_*)
}

trait ContractSpec extends DefaultContract with ContractSyntax

case class SpecContext(testSuite: SigmaTestingCommons)(implicit val IR: IRContext) {
  val builder: SigmaDslBuilder = new TestSigmaDslBuilder
  type PropositionFunc = Context => SigmaProp

  case class ErgoScript(env: ScriptEnv, code: String)

  case class PropositionSpec(name: String, dslSpec: PropositionFunc, scriptSpec: ErgoScript) {
    lazy val ergoTree: ErgoTree = testSuite.compileWithCosting(scriptSpec.env, scriptSpec.code)
  }

  trait ProtocolParty {
    def name: String
  }

  case class ProvingParty(name: String) extends ProtocolParty {
    private val prover = new ErgoLikeTestProvingInterpreter
    val pubKey: SigmaProp = ProveDlogEvidence(prover.dlogSecrets.head.publicImage.h)
  }

  case class VerifyingParty(name: String) extends ProtocolParty {
    private val verifier = new ErgoLikeTestInterpreter
  }

  implicit def Coll[T](items: Array[T])(implicit cT: RType[T]) = builder.Colls.fromArray(items)
}


case class Block(height: Int)(implicit specContext: SpecContext) {
  import specContext._
  
  def newBox(value: Long, propSpec: PropositionSpec): ErgoBox = ErgoBox(value, propSpec.ergoTree, height)
}
