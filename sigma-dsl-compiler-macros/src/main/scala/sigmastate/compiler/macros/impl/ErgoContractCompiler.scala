package sigmastate.compiler.macros.impl

import org.ergoplatform.Height
import sigmastate.Values.{ErgoTree, IntConstant, SValue}
import sigmastate._
import sigmastate.eval.CSigmaProp
import sigmastate.lang.Terms.ValueOps
import special.sigma.{Context, SigmaProp}

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.{Context => MacrosContext}

case class ErgoContract(scalaFunc: Context => SigmaProp, prop: SValue) {
  def ergoTree: ErgoTree = ErgoTree.fromProposition(prop.asSigmaProp)
}

object ErgoContractCompiler {
  def compile[A, B](verifiedContract: A => B): ErgoContract = macro ErgoContractCompilerImpl.compile[A, B]
}

class ErgoContractCompilerImpl(val c: MacrosContext) {
  import c.universe._

  private def error(str: String): Nothing = c.abort(c.enclosingPosition, str)

  private def buildFromScalaAst(s: Tree, defId: Int): Expr[SValue] = {
    def recurse[T <: SType](s: Tree) = buildFromScalaAst(s, defId)

    s match {
      case Block(stats, expr) =>
        // in stats "import ... " and "require"
        recurse(expr)
      case Apply(Select(_,TermName("sigmaProp")), args) =>
        reify(BoolToSigmaProp(recurse(args.head).splice.asBoolValue))
      case Apply(Select(lhs, TermName("$less")), args) =>
        val l = recurse(lhs)
        val r = recurse(args.head)
        reify(LT(l.splice, r.splice))
      case Select(_, TermName("HEIGHT")) =>
        reify(Height)
      case Ident(TermName("limit")) =>
        // TODO: don't hardcode parameter name
        val limTree = Ident(TermName("limit"))
        val limExpr = c.Expr[Int](limTree)
        reify(IntConstant(limExpr.splice))
      case _ => error(s"unexpected: $s")
    }
  }

  private def extractContractTree(select: Select): Tree = {
    import scala.meta._
    val path = select.symbol.pos.source.file.file.toPath
    val source = new String(java.nio.file.Files.readAllBytes(path), "UTF-8")
    val input = Input.VirtualFile(path.toString, source)
    val tree = input.parse[Source].get
    val closure = tree.collect {
      case defdef@Defn.Def(mods, name, tparams, paramss, decltpe, body) if name.toString == select.name.toString =>
        val defdefSource = defdef.toString
        val defSource =
          s"""
             |
             |{ ctx: special.sigma.Context =>
             |import special.sigma._
             |import org.ergoplatform.dsl._
             |
             |object SigmaContractHolder extends SigmaContractSyntax {
             |  lazy val spec = ???
             |  lazy val contractEnv = ???
             |
             |  $defdefSource
             |}
             |
             |SigmaContractHolder.contract(ctx, limit)
             |}
             |""".stripMargin
        // TODO: don't hardcode parameters into the call
        val scalaTree = c.parse(defSource)
        println(scalaTree)
        val scalaTreeTyped = c.typecheck(scalaTree)
        println(scalaTreeTyped)
        scalaTreeTyped
    }
    closure.head
  }

  def compile[A, B](verifiedContract: c.Expr[A => B]): c.Expr[ErgoContract] = {
    println(s"compile: ${showRaw(verifiedContract.tree)}")
    val selects = verifiedContract.tree.collect { case sel: Select => sel}
    if (selects.length != 1) c.abort(c.enclosingPosition, s"expected contract in form of a method")
    val contractTree = extractContractTree(selects.head)
    val defDefRhs = contractTree.collect { case DefDef(_, TermName("contract"), _, _, _, rhs) => rhs }
    val sigmaProp = reify(buildFromScalaAst(defDefRhs.head, 0).splice)
    reify(ErgoContract(c.Expr[Context => SigmaProp](contractTree).splice, sigmaProp.splice))
  }

}

