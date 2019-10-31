package sigmastate.compiler.macros.impl

import org.ergoplatform.Height
import sigmastate.{BoolToSigmaProp, GT, SInt, SSigmaProp, SType, TrivialProp, Values}
import sigmastate.Values.{BlockValue, ErgoTree, IntConstant, SValue, SigmaBoolean, SigmaPropConstant, SigmaPropValue, Value}
import sigmastate.eval.CSigmaProp
import sigmastate.lang.TransformingSigmaBuilder
import special.sigma.{Context, SigmaProp}

import scala.language.experimental.macros
import scala.meta.{Lit, Stat, Term}
import scala.reflect.macros.whitebox.{Context => MacrosContext}

case class ErgoContract(scalaFunc: Context => SigmaProp, prop: SigmaPropValue) {
  def ergoTree: ErgoTree = ErgoTree.fromProposition(prop)
}

object ErgoContractCompiler {
  def compile[A, B](verifiedContract: A => B): ErgoContract = macro ErgoContractCompilerImpl.compile[A, B]
}

class ErgoContractCompilerImpl(val c: MacrosContext) {
  import c.universe._

  private def error(str: String): Nothing = c.abort(c.enclosingPosition, str)

//  private val builder = TransformingSigmaBuilder

  private def buildFromScalametaAst(s: Stat, defId: Int): Expr[SValue] = {
//    import builder._
    import sigmastate.lang.Terms._
    def recurse[T <: SType](s: Stat) = buildFromScalametaAst(s, defId)

    s match {
      case Term.Block(stats) =>
        val expr = stats
          .tail // skip import ctx._
          .head
        recurse(expr)
      case Term.Apply(Term.Name("SigmaPropProof"), args) =>
        reify(BoolToSigmaProp(recurse(args.head).splice.asBoolValue))
      case Term.Apply(Term.Name("sigmaProp"), args) =>
        reify(BoolToSigmaProp(recurse(args.head).splice.asBoolValue))
      case Term.Apply(Term.Name("TrivialProp"), args) =>
        recurse(args.head)
      case Term.ApplyInfix(lhs, Term.Name(">"), targs, args) =>
        val l = recurse(lhs)
        val r = recurse(args.head)
        reify(GT(l.splice, r.splice))
      case Term.Name("HEIGHT") => reify(Height)
        // TODO: wtf wrong with v
      case Lit.Int(v) => reify(IntConstant(reify(1).splice))
      case _ => error(s"unexpected: $s")
    }
  }

  private def compileErgoTree(select: Select): Expr[SigmaPropValue] = {
    import scala.meta._

    val path = select.symbol.pos.source.file.file.toPath
    val source = new String(java.nio.file.Files.readAllBytes(path), "UTF-8")
    val input = Input.VirtualFile(path.toString, source)
    val tree = input.parse[Source].get
//    println(tree)

    val body = tree.collect {
      case defdef @ Defn.Def(mods, name, tparams, paramss, decltpe, body) if name.toString == select.name.toString =>
        val defdefSource = defdef.toString
        val defSource =
          s"""import special.sigma._
            |
            | object SigmaContractHolder extends SigmaContract {
            | $defdefSource
            | }
            |""".stripMargin
        val scalaTree = c.parse(defSource)
        println(scalaTree)
        val scalaTreeTyped = c.typecheck(scalaTree)
        println(scalaTreeTyped)
        body
    }
    reify(buildFromScalametaAst(body.head, 0).splice.asInstanceOf[Value[SSigmaProp.type]])
  }

  def compile[A, B](verifiedContract: c.Expr[A => B]): c.Expr[ErgoContract] = {

//    println(showRaw(verifiedContract.tree.tpe))
    println(s"compile: ${showRaw(verifiedContract.tree)}")

    // TODO scalaFun: in verifiedContract lambda find and inline call to the contract method with type conversion (from verified)
    val selects = verifiedContract.tree.collect { case sel: Select => sel}
    if (selects.length != 1) c.abort(c.enclosingPosition, s"expected contract in form of a method")
    // TODO ergo tree: capture parameter values and return scala code that will "embed" captured values into the tree
    val sigmaProp = compileErgoTree(selects.head)
//    val sigmaProp = reify(SigmaPropConstant(TrivialProp(true)))

    val contract = reify({c: Context => CSigmaProp(TrivialProp(true))})
    reify(ErgoContract(contract.splice, sigmaProp.splice))
  }

}

