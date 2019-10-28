package sigmastate.compiler.macros.impl

import org.ergoplatform.Height
import sigmastate.{SInt, SSigmaProp, SType, TrivialProp, Values}
import sigmastate.Values.{BlockValue, IntConstant, SValue, SigmaBoolean, SigmaPropConstant, SigmaPropValue, Value}
import sigmastate.eval.CSigmaProp
import sigmastate.lang.TransformingSigmaBuilder
import special.sigma.{Context, SigmaProp}

import scala.language.experimental.macros
import scala.meta.{Lit, Stat, Term}
import scala.reflect.macros.whitebox.{Context => MacrosContext}

case class CompilationResult(scalaFunc: Context => SigmaProp, prop: SigmaPropValue)

object ErgoContractCompiler {
  def compile[A, B](verifiedContract: A => B): CompilationResult = macro ErgoContractCompilerImpl.compile[A, B]
}

class ErgoContractCompilerImpl(val c: MacrosContext) {
  import c.universe._

  private def error(str: String): Nothing = c.abort(c.enclosingPosition, str)

  private val builder = TransformingSigmaBuilder

  private def buildValue(s: Stat, defId: Int): SValue = {
    import builder._
    import sigmastate.lang.Terms._
    def recurse[T <: SType](s: Stat) = buildValue(s, defId)

    s match {
      case Term.Block(stats) =>
        val expr = stats
          .tail // skip import ctx._
          .head
        buildValue(expr, defId)
      case Term.Apply(Term.Name("SigmaPropProof"), args) =>
        mkBoolToSigmaProp(buildValue(args.head, defId).asBoolValue)
      case Term.Apply(Term.Name("sigmaProp"), args) =>
        mkBoolToSigmaProp(buildValue(args.head, defId).asBoolValue)
      case Term.Apply(Term.Name("TrivialProp"), args) =>
        recurse(args.head).asBoolValue
      case Term.ApplyInfix(lhs, Term.Name(">"), targs, args) =>
        val l = recurse(lhs)
        val r = recurse(args.head)
        mkGT(l, r)
      case Term.Name("HEIGHT") => Height
      case Lit.Int(v) => mkConstant[SInt.type](v, SInt)
      case _ => error(s"unexpected: $s")
    }
  }

  private def compileErgoTree(select: Select): SigmaPropValue = {
    import scala.meta._

    val path = select.symbol.pos.source.file.file.toPath
    val input = Input.VirtualFile(path.toString, new String(java.nio.file.Files.readAllBytes(path), "UTF-8"))
    val tree = input.parse[Source].get
//    println(tree)

    val body = tree.collect {
      // TODO check enclosing type == select.qualifier
      // TODO check expected types
      case defdef @ Defn.Def(mods, name, tparams, paramss, decltpe, body) if name.toString == select.name.toString =>
        body
    }
//    println(body)
    val v = buildValue(body.head, 0)
    v.asInstanceOf[Value[SSigmaProp.type]]
  }

  def compile[A, B](verifiedContract: c.Expr[A => B]): c.Expr[CompilationResult] = {

//    println(showRaw(verifiedContract.tree.tpe))
//    println(showRaw(verifiedContract.tree))

    val selects = verifiedContract.tree.collect { case sel: Select => sel}
    if (selects.length != 1) c.abort(c.enclosingPosition, s"expected contract in form of a method")
    val sigmaProp = reify{ compileErgoTree(selects.head) }
//    val sigmaProp = reify(SigmaPropConstant(TrivialProp(true)))

    val contract = reify({c: Context => CSigmaProp(TrivialProp(true))})
    reify(CompilationResult(contract.splice, sigmaProp.splice))
  }

}

