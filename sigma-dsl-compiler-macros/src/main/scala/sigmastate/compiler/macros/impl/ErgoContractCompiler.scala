package sigmastate.compiler.macros.impl

import org.ergoplatform.ErgoBox.R4
import org.ergoplatform.{Height, Outputs}
import sigmastate.Values.{ByteConstant, ErgoTree, EvaluatedValue, IntConstant, LongConstant, SValue, SigmaPropConstant}
import sigmastate._
import sigmastate.lang.Terms.ValueOps
import sigmastate.utxo.{ByIndex, ExtractRegisterAs, OptionIsDefined, SelectField, SizeOf}
import special.sigma.{Context, SigmaProp}

import scala.language.experimental.macros
import scala.reflect.api.Trees
import scala.reflect.macros.TypecheckException
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

  @inline
  private def convertColl(paramName: String): c.Expr[EvaluatedValue[SCollection[SType]]] =
    c.Expr[EvaluatedValue[SCollection[SType]]](
    q"sigmastate.verified.VerifiedTypeConverters.VCollToErgoTree.to(${Ident(TermName(paramName))})"
  )

  @inline
  private def convertSigmaProp(paramName: String): c.Expr[SigmaProp] = c.Expr[SigmaProp](
    q"sigmastate.verified.VerifiedTypeConverters.VSigmaPropToSigmaProp.to(${Ident(TermName(paramName))})",
  )

  private def buildFromScalaAst(s: Tree, defId: Int, env: Map[String, Any], paramMap: Map[String, String]): Expr[SValue] = {
    import c.universe.definitions._

    def recurse[T <: SType](s: Tree) = buildFromScalaAst(s, defId, env, paramMap)

    def liftParam(n: String, tpe: Type): Expr[SValue] = tpe.widen match {
      case ByteTpe => reify(ByteConstant(c.Expr[Byte](Ident(TermName(paramMap(n)))).splice))
      case IntTpe => reify(IntConstant(c.Expr[Int](Ident(TermName(paramMap(n)))).splice))
      case LongTpe => reify(LongConstant(c.Expr[Long](Ident(TermName(paramMap(n)))).splice))
      case TypeRef(_, sym, List(_)) if sym.fullName == "special.collection.Coll" =>
        convertColl(paramMap(n))
      case TypeRef(_, sym, _) if sym.fullName == "special.sigma.SigmaProp" =>
        reify(SigmaPropConstant(convertSigmaProp(paramMap(n)).splice))
      case _ => error(s"unexpected ident type: $tpe")
    }

    s match {
      case Block(stats, expr) =>
        // in stats "import ... " and "require"
        recurse(expr)
      case Apply(Select(_,TermName("sigmaProp")), args) =>
        reify(BoolToSigmaProp(recurse(args.head).splice.asBoolValue))
      case Apply(Select(_, TermName("booleanToSigmaProp")), args) =>
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
      case Select(_, TermName("HEIGHT")) =>
        reify(Height)
      case Select(_, TermName("OUTPUTS")) =>
        reify(Outputs)
      case l@Literal(ct@Constant(i)) if ct.tpe == IntTpe =>
        reify(IntConstant(c.Expr[Int](l).splice))
      case Apply(Select(_, TermName("BooleanOps")), Seq(arg)) if arg.tpe == BooleanTpe =>
        reify(BoolToSigmaProp(recurse(arg).splice.asBoolValue))
      case Apply(Select(obj, TermName("apply")), args) =>
        reify(ByIndex(recurse(obj).splice.asCollection ,recurse(args.head).splice.asIntValue))
      case Select(obj, TermName("_1")) =>
        reify(SelectField(recurse(obj).splice.asTuple, 1))
      case Select(obj, m) =>
        val o = recurse(obj)
        obj.tpe.widen match {
          case TypeRef(_, sym, _) if sym.fullName == "special.collection.Coll" => m match {
            case TermName("length") => reify(SizeOf(o.splice.asCollection[SType]))
            case TermName("nonEmpty") => reify(GT(SizeOf(o.splice.asCollection[SType]), IntConstant(0)))
          }
          case TypeRef(_, sym, _) if sym.fullName == "scala.Option" => m match {
            case TermName("isDefined") => reify(OptionIsDefined(o.splice.asOption))
          }
          case v@_ => error(s"unexpected $obj(tpe: $v) select $m")
        }
      case Apply(TypeApply(sel@Select(obj, m), Seq(tArg)), _) =>
        // TODO: ensure it's an application of an implicit args
        val o = recurse(obj)
        obj.tpe.widen match {
          case TypeRef(_, sym, _) if sym.fullName == "special.sigma.Box" => m match {
              // TODO handle all registers
              // TODO recognize types
            case TermName("R4") => reify(ExtractRegisterAs(o.splice.asBox, R4, SOption(SCollection(SByte))))
          }
        }
      case Apply(Select(lhs, m), Seq(arg)) =>
        val l = recurse(lhs)
        val r = recurse(arg)
        // TODO: extractors
        lhs.tpe.widen match {
          case BooleanTpe => m match {
            case TermName("$amp$amp") =>
              reify(BinAnd(l.splice.asBoolValue, r.splice.asBoolValue))
            case TermName("$bar$bar") =>
              reify(BinOr(l.splice.asBoolValue, r.splice.asBoolValue))
          }
          case TypeRef(_, sym1, _) if sym1.fullName == "special.sigma.SigmaProp"
            || sym1.fullName.endsWith("BooleanOps") => m match {
            case TermName("$amp$amp") =>
              reify(SigmaAnd(l.splice.asSigmaProp, r.splice.asSigmaProp))
            case TermName("$bar$bar") =>
              reify(SigmaOr(l.splice.asSigmaProp, r.splice.asSigmaProp))
          }
          case _ => error(s"object $lhs(tpe: ${lhs.tpe.widen}) has unexpected $m with arg: $arg")
        }
      case i@Ident(TermName(n)) => env.get(n) match {
        case Some(v) => ???
        case None => liftParam(n, i.tpe)
      }
      case _ => error(s"unexpected: $s")
    }
  }

  private def findContractDefDef(select: Select): String = {
    import scala.meta._
    val path = select.symbol.pos.source.file.file.toPath
    val source = new String(java.nio.file.Files.readAllBytes(path), "UTF-8")
    val input = Input.VirtualFile(path.toString, source)
    val tree = input.parse[Source].get
    tree.collect {
      // TODO: check the full signature and not just the name
      case dd@Defn.Def(mods, name, tparams, paramss, decltpe, body) if name.toString == select.name.toString =>
        dd.toString
    }.headOption
      .getOrElse(error("cannot find DefDef for the contract method"))
  }

  private def buildScalaFunc(compilingClosure: Tree): Tree = {
    val select = compilingClosure.collect{ case sel: Select => sel }
      .headOption
      .getOrElse(error("method call for the contract is expected"))
    val defdefSource = findContractDefDef(select)
    val ctxParamName = compilingClosure.collect { case ValDef(mods, termName, _, _) => termName }
      .headOption
      .getOrElse(error("context parameter is expected"))
      .toString
    val compilingContractApp = compilingClosure.collect { case app: Apply => app }
      .headOption
      .getOrElse(error("cannot find Apply for the contract method"))
    val argsStr = compilingContractApp.args.flatMap(_.collect { case Ident(name) => name.toString })
      .mkString(",")
    if (argsStr.isEmpty) error("no arguments provided for the contract call")
    val scalaFuncSource =
      s"""
         |{ $ctxParamName: special.sigma.Context =>
         |import special.sigma._
         |import special.collection._
         |import org.ergoplatform.dsl._
         |import sigmastate.verified.VerifiedTypeConverters._
         |
         |object SigmaContractHolder extends SigmaContractSyntax {
         |  import syntax._
         |  lazy val spec = ???
         |  lazy val contractEnv = ???
         |
         |  //implicit def booleanToSigmaProp(source: Boolean): SigmaProp = this.builder.sigmaProp(source)
         |
         |  $defdefSource
         |}
         |
         |SigmaContractHolder.${select.name.toString}($argsStr)
         |}
         |""".stripMargin
    val tree = c.parse(scalaFuncSource)
    try {
      c.typecheck(tree)
    } catch {
      case e: TypecheckException =>
        error(s"Failed to typecheck with error: $e\n for source:\n $scalaFuncSource \n for tree: ${showRaw(tree)}")
    }
  }

  def compile[A, B](verifiedContract: c.Expr[A => B]): c.Expr[ErgoContract] = {
    println(s"compile: ${showRaw(verifiedContract.tree)}")
    val contractMethodName = verifiedContract.tree.collect { case sel: Select => sel }
      .headOption
      .getOrElse(error("method call for the contract is expected"))
      .name.toString
    val contractTree = buildScalaFunc(verifiedContract.tree)
    val defDef = contractTree.collect { case dd @ DefDef(_, TermName(`contractMethodName`), _, _, _, _) => dd }
      .headOption
      .getOrElse(error("cannot find DefDef for the contract method"))
    val compilingContractApp = verifiedContract.tree.collect { case app: Apply => app }
      .headOption
      .getOrElse(error("cannot find Apply for the contract method"))
    val appArgs = compilingContractApp.args.collect { case Ident(name) => name.toString }
    val defDefArgNames = defDef.vparamss.head.collect { case ValDef(_, name, _, _) => name.toString }
    val paramMap = defDefArgNames.zip(appArgs).toMap
    val sigmaProp = reify(buildFromScalaAst(defDef.rhs, 0, Map(), paramMap).splice)
    reify(ErgoContract(c.Expr[Context => SigmaProp](contractTree).splice, sigmaProp.splice))
  }

}

