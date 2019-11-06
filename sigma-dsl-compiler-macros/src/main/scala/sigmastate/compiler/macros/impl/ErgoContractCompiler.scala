package sigmastate.compiler.macros.impl

import org.ergoplatform.Height
import sigmastate.Values.{ByteArrayConstant, ByteConstant, ErgoTree, IntConstant, LongConstant, SValue}
import sigmastate._
import sigmastate.lang.Terms.ValueOps
import sigmastate.utxo.SizeOf
import special.collection.Coll
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

  private def buildFromScalaAst(s: Tree, defId: Int, env: Map[String, Any], paramMap: Map[String, String]): Expr[SValue] = {
    import c.universe.definitions._

    def recurse[T <: SType](s: Tree) = buildFromScalaAst(s, defId, env, paramMap)

    def liftParam(n: String, tpe: Type): Expr[SValue] = tpe match {
      case ByteTpe => reify(ByteConstant(c.Expr[Byte](Ident(TermName(paramMap(n)))).splice))
      case IntTpe => reify(IntConstant(c.Expr[Int](Ident(TermName(paramMap(n)))).splice))
      case LongTpe => reify(LongConstant(c.Expr[Long](Ident(TermName(paramMap(n)))).splice))
      // TODO: byte array
      case TypeRef(pre, sym, List(targ)) if sym.fullName == "special.collection.Coll" && targ == ByteTpe =>
        // TODO convert n from verified Coll
        reify(ByteArrayConstant(
          c.Expr[Coll[Byte]](
            Apply(
              q"sigmastate.verification.contract.VerifiedConvertors.verifiedCollToColl",
              List(Ident(TermName(paramMap(n))))
            )
          ).splice
        ))
//        error(s"ident type: ${sym.fullName}")
      case SingleType(_, sym) if sym.isTerm =>
        liftParam(n, tpe.widen)
      // TODO: PK
      case _ => error(s"unexpected ident type: $tpe")
    }

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
      case Apply(Select(lhs, TermName("$greater")), args) =>
        val l = recurse(lhs)
        val r = recurse(args.head)
        reify(GT(l.splice, r.splice))
      case Apply(Select(lhs, TermName("$less$eq")), args) =>
        val l = recurse(lhs)
        val r = recurse(args.head)
        reify(LE(l.splice, r.splice))
      case Apply(Select(lhs, TermName("$greater$eq")), args) =>
        val l = recurse(lhs)
        val r = recurse(args.head)
        reify(GE(l.splice, r.splice))
      case Apply(Select(lhs, TermName("$amp$amp")), args) =>
        val l = recurse(lhs)
        val r = recurse(args.head)
        reify(BinAnd(l.splice.asBoolValue, r.splice.asBoolValue))
      case Select(_, TermName("HEIGHT")) =>
        reify(Height)
      case Select(obj, TermName("length")) =>
        val o = recurse(obj)
        reify(SizeOf(o.splice.asCollection[SType]))
      case i @ Ident(TermName(n)) => env.get(n) match {
        case Some(v) => ???
        case None => liftParam(n, i.tpe)
      }
      case l@Literal(ct@Constant(i)) if ct.tpe == IntTpe =>
        reify(IntConstant(c.Expr[Int](l).splice))
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
         |import special.collection._
         |import org.ergoplatform.dsl._
         |import sigmastate.verification.contract.VerifiedConvertors._
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
    val contractFuncName = verifiedContract.tree.collect { case sel: Select => sel }.head.name.toString
    val contractTree = buildScalaFunc(verifiedContract.tree)
    val defDef = contractTree.collect { case dd @ DefDef(_, TermName(`contractFuncName`), _, _, _, _) => dd }.head
    val compilingContractApp = verifiedContract.tree.collect { case app: Apply => app }.head
    val appArgs = compilingContractApp.args.collect { case Ident(name) => name.toString }
    val defDefArgNames = defDef.vparamss.head.collect { case ValDef(_, name, _, _) => name.toString }
    val paramMap = defDefArgNames.zip(appArgs).toMap
    val sigmaProp = reify(buildFromScalaAst(defDef.rhs, 0, Map(), paramMap).splice)
    reify(ErgoContract(c.Expr[Context => SigmaProp](contractTree).splice, sigmaProp.splice))
  }

}

