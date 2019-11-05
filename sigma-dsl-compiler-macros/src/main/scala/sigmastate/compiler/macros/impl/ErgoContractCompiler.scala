package sigmastate.compiler.macros.impl

import com.sun.source.util.Trees
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

  private def findContractDefDef(select: Select): String = {
    import scala.meta._
    val path = select.symbol.pos.source.file.file.toPath
    val source = new String(java.nio.file.Files.readAllBytes(path), "UTF-8")
    val input = Input.VirtualFile(path.toString, source)
    val tree = input.parse[Source].get
    val results = tree.collect {
      // TODO: check the full signature and not just the name
      case defdef@Defn.Def(mods, name, tparams, paramss, decltpe, body) if name.toString == select.name.toString =>
        defdef.toString
    }
    results.head
  }

  private def buildScalaFunc(compilingClosure: Tree): Tree = {
    val select = compilingClosure.collect{ case sel: Select => sel }.head
    val defdefSource = findContractDefDef(select)
    val ctxParamName = compilingClosure.collect { case ValDef(mods, termName, _, _) => termName }
      .headOption
      .getOrElse(error("context parameter is expected"))
      .toString
    val compilingContractApp = compilingClosure.collect { case app: Apply => app }.head
    val argsStr = compilingContractApp.args.collect { case Ident(name) => name.toString}
      .mkString(",")
    val scalaFuncSource =
      s"""
         |{ $ctxParamName: special.sigma.Context =>
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
         |SigmaContractHolder.${select.name.toString}($argsStr)
         |}
         |""".stripMargin
    c.typecheck(c.parse(scalaFuncSource))
  }

  def compile[A, B](verifiedContract: c.Expr[A => B]): c.Expr[ErgoContract] = {
    println(s"compile: ${showRaw(verifiedContract.tree)}")
    val contractTree = buildScalaFunc(verifiedContract.tree)
    val defDefRhs = contractTree.collect { case DefDef(_, TermName("contract"), _, _, _, rhs) => rhs }
    val sigmaProp = reify(buildFromScalaAst(defDefRhs.head, 0).splice)
    reify(ErgoContract(c.Expr[Context => SigmaProp](contractTree).splice, sigmaProp.splice))
  }

}

