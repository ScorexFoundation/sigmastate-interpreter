package org.ergoplatform.dsl

import org.ergoplatform.ErgoBox.TokenId
import scalan.RType
import sigmastate.SType
import sigmastate.SType.AnyOps
import org.ergoplatform.dsl.ContractSyntax.{ErgoScript, Proposition}
import sigmastate.eval.{CostingSigmaDslBuilder, Evaluation}
import sigmastate.interpreter.Interpreter.ScriptEnv
import special.sigma.{SigmaProp, SigmaContract, Context, SigmaDslBuilder}
import scala.language.implicitConversions

/** Defines methods to be used in contract implementations based on [[SigmaContract]]. */
trait ContractSyntax { contract: SigmaContract =>
  override def builder: SigmaDslBuilder = CostingSigmaDslBuilder

  /** Instance of contract specification DSL, which can be imported in the body of
   * [[SigmaContract]] implementations. */
  val spec: ContractSpec

  /** A contract environment which defines named constants used in the contract.
    * Should be defined in [[SigmaContract]] implementations.
    */
  def contractEnv: ScriptEnv

  /** The default verifier which represents miner's role in verification of transactions.
    * It can be overriden in derived classes. */
  lazy val verifier: spec.VerifyingParty = spec.VerifyingParty("Miner")

  /** Helper method to support Scala <-> ErgoScript equivalence. */
  def Coll[T](items: T*)(implicit cT: RType[T]) = builder.Colls.fromItems(items:_*)

  /** Call this function in [[SigmaContract]] implementations to define propositions.
    *
    * @param name    name of the proposition (aka contract name)
    * @param dslSpec Scala lambda of type [[Proposition]] which defines contract semantics
    *                and can be executed directly.
    * @param scriptCode ErgoScript representation of the contract.
    * @param scriptVersion optional script version to be used in ErgoTree.
    *                      If None then ergoTreeVersionInTests is used.
    * @return proposition specification with compiled ErgoTree.
    */
  def proposition(name: String,
                  dslSpec: Proposition,
                  scriptCode: String,
                  scriptVersion: Option[Byte] = None): spec.PropositionSpec = {
    val env = contractEnv.map { case (k, v) =>
      val tV = Evaluation.rtypeOf(v).get
      val elemTpe = Evaluation.rtypeToSType(tV)
      k -> spec.IR.builder.mkConstant[SType](v.asWrappedType, elemTpe)
    }.toMap
    spec.mkPropositionSpec(name, dslSpec, ErgoScript(env, scriptCode, scriptVersion))
  }

  /** Creates new environment with the given named constants. */
  def Env(entries: (String, Any)*): ScriptEnv = Map(entries:_*)
}
object ContractSyntax {
  /** Type of proposition as Scala lambda. */
  type Proposition = Context => SigmaProp
  /** Represents ErgoScript contract to be compiled. */
  case class ErgoScript(env: ScriptEnv, code: String, scriptVersion: Option[Byte])
  /** Typed representation of token id and amount. */
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






