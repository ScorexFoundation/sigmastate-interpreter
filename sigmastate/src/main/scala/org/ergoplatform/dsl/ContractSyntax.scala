package org.ergoplatform.dsl

import scalan.RType
import sigmastate.SType
import sigmastate.SType.AnyOps
import org.ergoplatform.dsl.ContractSyntax.{ErgoScript, Proposition}
import sigmastate.eval.{CostingSigmaDslBuilder, Evaluation}
import sigmastate.interpreter.Interpreter.ScriptEnv
import special.collection.Coll
import special.sigma.{SigmaProp, SigmaContract, Context, SigmaDslBuilder}
import scala.language.implicitConversions

trait ContractSyntax { contract: SigmaContract =>
  override def builder: SigmaDslBuilder = CostingSigmaDslBuilder
  val spec: ContractSpec
  def contractEnv: ScriptEnv

  /** The default verifier which represents miner's role in verification of transactions.
    * It can be overriden in derived classes. */
  lazy val verifier: spec.VerifyingParty = spec.VerifyingParty("Miner")

  def Coll[T](items: T*)(implicit cT: RType[T]) = builder.Colls.fromItems(items:_*)

  def proposition(name: String,
                  dslSpec: Proposition,
                  scriptCode: String,
                  scriptVersion: Option[Byte] = None): spec.PropositionSpec = {
    val env = contractEnv.mapValues { v =>
      val tV = Evaluation.rtypeOf(v).get
      val elemTpe = Evaluation.rtypeToSType(tV)
      spec.IR.builder.mkConstant[SType](v.asWrappedType, elemTpe)
    }
    spec.mkPropositionSpec(name, dslSpec, ErgoScript(env, scriptCode, scriptVersion))
  }

  def Env(entries: (String, Any)*): ScriptEnv = Map(entries:_*)
}
object ContractSyntax {
  type Proposition = Context => SigmaProp
  type TokenId = Coll[Byte]
  case class ErgoScript(env: ScriptEnv, code: String, scriptVersion: Option[Byte])
  case class Token(id: TokenId, value: Long)
}

trait SigmaContractSyntax extends SigmaContract with ContractSyntax {

  implicit class BooleanSyntax(source: Boolean) {
    /** Logical AND between Boolean on the left and SigmaProp value on the right. */
    def &&(prop: SigmaProp) = builder.sigmaProp(source) && prop

    /** Logical AND between Boolean on the left and SigmaProp value on the right. */
    def ||(prop: SigmaProp) = builder.sigmaProp(source) || prop
  }

}






