package org.ergoplatform.dsl

import scalan.RType
import sigmastate.SType
import sigmastate.SType.AnyOps
import org.ergoplatform.dsl.ContractSyntax.{ErgoScript, Proposition}
import sigmastate.eval.{CostingSigmaDslBuilder, Evaluation}
import sigmastate.interpreter.Interpreter.ScriptEnv
import special.collection.Coll
import special.sigma.{SigmaProp, SigmaContract, Context, DslSyntaxExtensions, SigmaDslBuilder}
import scala.language.implicitConversions

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
      val elemTpe = Evaluation.rtypeToSType(tV)
      spec.IR.builder.mkConstant[SType](v.asWrappedType, elemTpe)
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






