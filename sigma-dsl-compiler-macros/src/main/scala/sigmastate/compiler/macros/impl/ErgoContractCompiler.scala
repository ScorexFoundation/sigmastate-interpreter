package sigmastate.compiler.macros.impl

import org.ergoplatform.ErgoBox.{R2, R4}
import org.ergoplatform.{ErgoBox, Height, Outputs, Self}
import sigmastate.Values.{BlockItem, BlockValue, ByteConstant, ErgoTree, EvaluatedValue, IntConstant, LongConstant, SValue, SigmaPropConstant, ValUse}
import sigmastate._
import sigmastate.lang.Terms.ValueOps
import sigmastate.utxo.{ByIndex, ExtractId, ExtractRegisterAs, ExtractScriptBytes, OptionGet, OptionIsDefined, SelectField, SigmaPropBytes, SizeOf}
import special.sigma.{Context, SigmaProp}

import scala.collection.mutable.ArrayBuffer
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
  import c.universe.definitions._

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

  def tpeToSType(tpe: Type): SType = tpe.widen match {
    case BooleanTpe => SBoolean
    case ByteTpe => SByte
    case LongTpe => SLong
    case TypeRef(_, sym, List(arg)) if sym.fullName == "special.collection.Coll" =>
      SCollectionType(tpeToSType(arg))
    case TypeRef(_, sym, targs) if sym.fullName == "scala.Tuple2" =>
      STuple(targs.map(tpeToSType).toIndexedSeq)
    case v@_ => error(s"cannot convert tpe $v to SType")
  }

  def sTypeExpr(tpe: SType): c.Expr[SType] = tpe match {
    case SBoolean => reify(SBoolean)
    case SByte => reify(SByte)
    case SInt => reify(SInt)
    case SLong => reify(SLong)
    case SCollectionType(eT) => reify(SCollectionType(sTypeExpr(eT).splice))
    case STuple(items) =>
      val itemExprs = items.map(sTypeExpr)
      reify(STuple(sequenceExpr(itemExprs).splice.toIndexedSeq))
    case v@_ => error(s"cannot convert SType $v to tree")
  }

  private def sequenceExpr[A](xs: Seq[c.Expr[A]]): c.Expr[List[A]] =
    c.Expr[List[A]](
      xs.foldRight(Ident(NilModule): Tree) { (el, acc) =>
        Apply(Select(acc, TermName("$colon$colon")), List(el.tree))
      }
    )

  def liftParam(n: String, tpe: Type, paramMap: Map[String, String]): Expr[SValue] = tpe.widen match {
    case ByteTpe => reify(ByteConstant(c.Expr[Byte](Ident(TermName(paramMap(n)))).splice))
    case IntTpe => reify(IntConstant(c.Expr[Int](Ident(TermName(paramMap(n)))).splice))
    case LongTpe => reify(LongConstant(c.Expr[Long](Ident(TermName(paramMap(n)))).splice))
    case TypeRef(_, sym, List(_)) if sym.fullName == "special.collection.Coll" =>
      convertColl(paramMap(n))
    case TypeRef(_, sym, _) if sym.fullName == "special.sigma.SigmaProp" =>
      reify(SigmaPropConstant(convertSigmaProp(paramMap(n)).splice))
    case _ => error(s"unexpected ident type: $tpe")
  }

  private def buildFromScalaAst(s: Tree, defId: Int, paramMap: Map[String, String], valDefNameIds: Map[String, (Int, SType)]): Expr[SValue] = {

    def recurse[T <: SType](s: Tree) =
      buildFromScalaAst(s, defId, paramMap, valDefNameIds)

    s match {
      case Block(stats, expr) if !stats.exists(_.isInstanceOf[ValDef]) =>
        reify(recurse(expr).splice)

//      case Block(stats, expr) =>
//        // TODO rewrite without nested BlockValue's (use ValUse in subsequent ValDef references)
//        stats.filter(_.isInstanceOf[ValDef]).map(_.asInstanceOf[ValDef]) match {
//          case h :: t =>
//            reify(
//              BlockValue(
//                IndexedSeq(
//                  Values.ValDef(
//                    c.Expr[Int](Literal(Constant(defId + 1))).splice,
//                    buildFromScalaAst(h.rhs,
//                      defId + 1,
//                      env, paramMap, valDefNameIds, valDefs).splice
//                  )
//                ),
//                buildFromScalaAst(
//                  Block(t, expr),
//                  defId + 1,
//                  env,
//                  paramMap,
//                  valDefNameIds + (h.name.toString -> (
//                    defId + 1,
//                    tpeToSType(h.rhs.tpe)
//                  )),
//                  valDefs
//                ).splice
//              )
//            )
//        }

      case Block(stats, expr) =>
        val (lastVdExprs, lastId, lastVdIds) = stats
          .filter(_.isInstanceOf[ValDef])
          .foldLeft((List.empty[Expr[Values.ValDef]], defId, valDefNameIds)){
            case ((valDefsExpr, lastUsedId, vdIds), ValDef(_, TermName(n), tpt, rhs)) =>
              val curId = lastUsedId + 1
              (valDefsExpr :+ reify(
                Values.ValDef(
                  c.Expr[Int](Literal(Constant(curId))).splice,
                  buildFromScalaAst(rhs,
                    curId,
                    paramMap,
                    vdIds + (n -> (curId, tpeToSType(rhs.tpe)))
                  ).splice
                )
              ),
                curId,
                vdIds + (n -> (curId, tpeToSType(rhs.tpe)))
              )
          }
        reify(
          BlockValue(
            sequenceExpr(lastVdExprs).splice.toIndexedSeq,
            buildFromScalaAst(expr, lastId, paramMap, lastVdIds).splice
          )
        )

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

      case Select(_, TermName("SELF")) =>
        reify(Self)

      case l@Literal(ct@Constant(i)) if ct.tpe == IntTpe =>
        reify(IntConstant(c.Expr[Int](l).splice))

      case Apply(Select(_, TermName("BooleanOps")), Seq(arg)) if arg.tpe == BooleanTpe =>
        reify(BoolToSigmaProp(recurse(arg).splice.asBoolValue))

      case Apply(Select(obj, TermName("apply")), args) =>
        val o = recurse(obj)
        reify(ByIndex(o.splice.asCollection ,recurse(args.head).splice.asIntValue))

      case Select(obj, TermName("_1")) =>
        reify(SelectField(recurse(obj).splice.asTuple, 1))

      case Select(obj, TermName("_2")) =>
        reify(SelectField(recurse(obj).splice.asTuple, 2))

      case Select(obj, m) =>
        val o = recurse(obj)
        obj.tpe.widen match {
          case TypeRef(_, sym, _) if sym.fullName == "special.collection.Coll" => m match {
            case TermName("length") => reify(SizeOf(o.splice.asCollection[SType]))
            case TermName("nonEmpty") => reify(GT(SizeOf(o.splice.asCollection[SType]), IntConstant(0)))
          }
          case TypeRef(_, sym, _) if sym.fullName == "special.sigma.Box" => m match {
            case TermName("tokens") =>
              // TODO check how it's done in sigma
              reify(OptionGet(ExtractRegisterAs(o.splice.asBox, R2, SOption(ErgoBox.STokensRegType))))
            case TermName("propositionBytes") =>
              reify(ExtractScriptBytes(o.splice.asBox))
            case TermName("id") =>
              reify(ExtractId(o.splice.asBox))
          }
          case TypeRef(_, sym, _) if sym.fullName == "special.sigma.SigmaProp" => m match {
            case TermName("propBytes") =>
              reify(SigmaPropBytes(o.splice.asSigmaProp))
          }
          case TypeRef(_, sym, _) if sym.fullName == "scala.Option" => m match {
            case TermName("isDefined") => reify(OptionIsDefined(o.splice.asOption))
            case TermName("get") => reify(OptionGet(o.splice.asOption))
          }
          case v@_ => error(s"unexpected $obj(tpe: $v) select $m")
        }

      case Apply(TypeApply(sel@Select(obj, m), Seq(tArg)), _) =>
        // TODO: ensure it's an application of implicit args
        val o = recurse(obj)
        obj.tpe.widen match {
          case TypeRef(_, sym, _) if sym.fullName == "special.sigma.Box" => m match {
              // TODO handle all registers
              // TODO recognize types
            case TermName("R4") => reify(ExtractRegisterAs(o.splice.asBox, R4, SOption(SCollection(SByte))))
          }
        }

      case Apply(Select(lhs, TermName("$eq$eq")), Seq(arg)) =>
        val l = recurse(lhs)
        val r = recurse(arg)
        reify(EQ(l.splice, r.splice))

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

      case i@Ident(TermName(n)) =>
        valDefNameIds.get(n) match {
          case Some(v) =>
            reify(ValUse(
              c.Expr[Int](Literal(Constant(v._1))).splice,
              sTypeExpr(v._2).splice
            ))
          case None => liftParam(n, i.tpe, paramMap)
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
    val appArgs = compilingContractApp.args.flatMap(_.collect { case Ident(name) => name.toString })

    val defDefArgNames = defDef.vparamss.head.collect { case ValDef(_, name, _, _) => name.toString }
    val paramMap = defDefArgNames.zip(appArgs).toMap
    val sigmaProp = reify(buildFromScalaAst(defDef.rhs, 0, paramMap, Map()).splice)
    reify(ErgoContract(c.Expr[Context => SigmaProp](contractTree).splice, sigmaProp.splice))
  }

}

