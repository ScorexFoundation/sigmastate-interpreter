package sigmastate.lang

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.ergoplatform._
import sigmastate.SCollection._
import sigmastate.Values._
import sigmastate._
import SCollection.SBooleanArray
import scalan.Nullable
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions._
import sigmastate.lang.SigmaPredef._
import sigmastate.serialization.OpCodes
import sigmastate.utxo._
import sigma.util.Extensions._

import scala.collection.mutable.ArrayBuffer

/**
  * Type inference and analysis for Sigma expressions.
  */
class SigmaTyper(val builder: SigmaBuilder, predefFuncRegistry: PredefinedFuncRegistry) {
  import SigmaTyper._
  import builder._
  import predefFuncRegistry._

  private implicit val implicitPredefFuncRegistry: PredefinedFuncRegistry = predefFuncRegistry

  private val tT = STypeIdent("T") // to be used in typing rules

  private val predefinedEnv: Map[String, SType] =
      predefFuncRegistry.funcs.map(f => f.name -> f.declaration.tpe).toMap

  private def processGlobalMethod(srcCtx: Nullable[SourceContext], method: SMethod) = {
    val global = Global.withPropagatedSrcCtx(srcCtx)
    val node = for {
      pf <- method.irBuilder
      res <- pf.lift((builder, global, method, IndexedSeq(), emptySubst))
    } yield res
    node.getOrElse(mkMethodCall(global, method, IndexedSeq(), emptySubst).withPropagatedSrcCtx(srcCtx))
  }
  /**
    * Rewrite tree to typed tree.  Checks constituent names and types.  Uses
    * the env map to resolve bound variables and their types.
    */
  def assignType(env: Map[String, SType],
                 bound: SValue,
                 expected: Option[SType] = None): SValue = ( bound match {
    case Block(bs, res) =>
      var curEnv = env
      val bs1 = ArrayBuffer[Val]()
      for (v @ Val(n, _, b) <- bs) {
        if (curEnv.contains(n)) error(s"Variable $n already defined ($n = ${curEnv(n)}", v.sourceContext)
        val b1 = assignType(curEnv, b)
        curEnv = curEnv + (n -> b1.tpe)
        builder.currentSrcCtx.withValue(v.sourceContext) {
          bs1 += mkVal(n, b1.tpe, b1)
        }
      }
      val res1 = assignType(curEnv, res)
      mkBlock(bs1, res1)

    case Tuple(items) =>
      mkTuple(items.map(assignType(env, _)))

    case c @ ConcreteCollection(items, _) =>
      val newItems = items.map(assignType(env, _))
      assignConcreteCollection(c, newItems)

    case i @ Ident(n, _) =>
      env.get(n) match {
        case Some(t) => mkIdent(n, t)
        case None =>
          SGlobal.method(n) match {
            case Some(method) if method.stype.tDom.length == 1 => // this is like  `groupGenerator` without parentheses
              val srcCtx = i.sourceContext
              processGlobalMethod(srcCtx, method)
            case _ =>
              error(s"Cannot assign type for variable '$n' because it is not found in env $env", bound.sourceContext)
          }
      }

    case sel @ Select(obj, n, None) =>
      val newObj = assignType(env, obj)
      newObj.tpe match {
        case tNewObj: SProduct =>
          val iField = tNewObj.methodIndex(n)
          val method = if (iField != -1) {
            tNewObj.methods(iField)
          } else
            throw new MethodNotFound(s"Cannot find method '$n' in in the object $obj of Product type with methods ${tNewObj.methods}", obj.sourceContext.toOption)
          val tMeth = method.stype
          val tRes = tMeth match {
            case SFunc(args, _, _) =>
              val tThis = args(0)  // first arg corresponds to method's receiver
              val tMethSpec = unifyTypes(tThis, tNewObj) match {
                case Some(subst) if subst.nonEmpty => applySubst(tMeth, subst).asFunc // specialize for concrete receiver type
                case _ => tMeth.asFunc
              }
              if (tMethSpec.tDom.length == 1 && tMethSpec.tpeParams.isEmpty) tMethSpec.tRange
              else tMethSpec.copy(tDom = tMethSpec.tDom.tail, tRange = tMethSpec.tRange)
            case _ => tMeth
          }
          if (method.irBuilder.isDefined && !tRes.isFunc) {
            // this is MethodCall of parameter-less property, so invoke builder and/or fallback to just MethodCall
            val methodConcrType = method.withSType(SFunc(newObj.tpe, tRes))
            methodConcrType.irBuilder.flatMap(_.lift(builder, newObj, methodConcrType, IndexedSeq(), Map()))
                .getOrElse(mkMethodCall(newObj, methodConcrType, IndexedSeq(), Map()))
          } else {
            mkSelect(newObj, n, Some(tRes))
          }
        case t =>
          error(s"Cannot get field '$n' in in the object $obj of non-product type $t", sel.sourceContext)
      }

    case lam @ Lambda(tparams, args, t, body) =>
      for ((name, t) <- args)
        if (t == NoType)
          error(s"Invalid function $lam: undefined type of argument $name", lam.sourceContext)
      val lambdaEnv = env ++ args
      val newBody = body.map(assignType(lambdaEnv, _))
      if (t != NoType) {
        if (newBody.isDefined && t != newBody.get.tpe)
          error(s"Invalid function $lam: resulting expression type ${newBody.get.tpe} doesn't equal declared type $t", lam.sourceContext)
      }
      val res = mkGenLambda(tparams, args, newBody.fold(t)(_.tpe), newBody)
      res

    case Apply(ApplyTypes(sel @ Select(obj, n, _), Seq(rangeTpe)), args) =>
      val newObj = assignType(env, obj)
      val newArgs = args.map(assignType(env, _))
      obj.tpe match {
        case p: SProduct =>
          p.method(n) match {
            case Some(method @ SMethod(_, _, genFunTpe @ SFunc(_, _, _), _, _)) =>
              val subst = Map(genFunTpe.tpeParams.head.ident -> rangeTpe)
              val concrFunTpe = applySubst(genFunTpe, subst)
              val expectedArgs = concrFunTpe.asFunc.tDom.tail
              val newArgTypes = newArgs.map(_.tpe)
              if (expectedArgs.length != newArgTypes.length
                || !expectedArgs.zip(newArgTypes).forall { case (ea, na) => ea == SAny || ea == na })
                error(s"For method $n expected args: $expectedArgs; actual: $newArgTypes", sel.sourceContext)
              if (method.irBuilder.isDefined) {
                method.irBuilder.flatMap(_.lift(builder, newObj, method, newArgs, subst))
                  .getOrElse(mkMethodCall(newObj, method, newArgs, subst))
              } else {
                val newSelect = mkSelect(newObj, n, Some(concrFunTpe)).withSrcCtx(sel.sourceContext)
                mkApply(newSelect, newArgs)
              }
            case Some(method) =>
              error(s"Don't know how to handle method $method in obj $p", sel.sourceContext)
            case None =>
              throw new MethodNotFound(s"Cannot find method '$n' in in the object $obj of Product type with methods ${p.methods}", obj.sourceContext.toOption)
          }
        case _ =>
          error(s"Cannot get field '$n' in in the object $obj of non-product type ${obj.tpe}", sel.sourceContext)
      }

    case app @ Apply(sel @ Select(obj, n, _), args) =>
      val newSel = assignType(env, sel)
      val newArgs = args.map(assignType(env, _))
      newSel.tpe match {
        case genFunTpe @ SFunc(argTypes, _, _) =>
          // If it's a function then the application has type of that function's return type.
          val newObj = assignType(env, obj)
          val newArgTypes = newArgs.map(_.tpe)
          unifyTypeLists(argTypes, newArgTypes) match {
            case Some(subst) =>
              val concrFunTpe = applySubst(genFunTpe, subst)
              newObj.tpe.asInstanceOf[SProduct].method(n) match {
                case Some(method) if method.irBuilder.isDefined =>
                  val expectedArgs = concrFunTpe.asFunc.tDom
                  if (expectedArgs.length != newArgTypes.length
                    || !expectedArgs.zip(newArgTypes).forall { case (ea, na) => ea == SAny || ea == na })
                    error(s"For method $n expected args: $expectedArgs; actual: $newArgTypes", sel.sourceContext)
                  val methodConcrType = method.withSType(concrFunTpe.asFunc.withReceiverType(newObj.tpe))
                  methodConcrType.irBuilder.flatMap(_.lift(builder, newObj, methodConcrType, newArgs, Map()))
                    .getOrElse(mkMethodCall(newObj, methodConcrType, newArgs, Map()))
                case _ =>
                  val newSelect = mkSelect(newObj, n, Some(concrFunTpe)).withSrcCtx(sel.sourceContext)
                  mkApply(newSelect, newArgs)
              }
            case None =>
              error(s"Invalid argument type of application $app: expected $argTypes; actual: $newArgTypes", sel.sourceContext)
          }
        case _ =>
          mkApply(newSel, newArgs)
      }

    case a @ Apply(ident: Ident, args) if SGlobal.hasMethod(ident.name) => // example: groupGenerator()
      val method = SGlobal.method(ident.name).get
      val srcCtx = a.sourceContext
      processGlobalMethod(srcCtx, method)

    case app @ Apply(f, args) =>
      val new_f = assignType(env, f)
      (new_f.tpe match {
        case SFunc(argTypes, _, _) =>
          // If it's a pre-defined function application
          if (args.length != argTypes.length)
            error(s"Invalid argument type of application $app: invalid number of arguments", app.sourceContext)
          val typedArgs = args.zip(argTypes).map {
            case (arg, expectedType) => assignType(env, arg, Some(expectedType))
          }
          val adaptedTypedArgs = (new_f, typedArgs) match {
            case (AllOfFunc.sym | AnyOfFunc.sym, _) =>
              adaptSigmaPropToBoolean(typedArgs, argTypes)
            case (Ident(GetVarFunc.name | ExecuteFromVarFunc.name, _), Seq(id: Constant[SNumericType]@unchecked))
              if id.tpe.isNumType =>
                Seq(ByteConstant(SByte.downcast(id.value.asInstanceOf[AnyVal])).withSrcCtx(id.sourceContext))
            case _ => typedArgs
          }
          val actualTypes = adaptedTypedArgs.map(_.tpe)
          if (actualTypes != argTypes)
            error(s"Invalid argument type of application $app: expected $argTypes; actual after typing: $actualTypes", app.sourceContext)
          mkApply(new_f, adaptedTypedArgs.toIndexedSeq)
        case _: SCollectionType[_] =>
          // If it's a collection then the application has type of that collection's element.
          args match {
            case Seq(c @ Constant(index, _: SNumericType)) =>
              val indexConst = IntConstant(SInt.upcast(index.asInstanceOf[AnyVal])).withSrcCtx(c.sourceContext)
              mkByIndex[SType](new_f.asCollection, indexConst, None)
            case Seq(index) =>
              val typedIndex = assignType(env, index)
              typedIndex.tpe match {
                case _: SNumericType =>
                  mkByIndex[SType](new_f.asCollection, typedIndex.upcastTo(SInt), None)
                case _ =>
                  error(s"Invalid argument type of array application $app: expected numeric type; actual: ${typedIndex.tpe}", index.sourceContext)
              }
            case _ =>
              error(s"Invalid argument of array application $app: expected integer value; actual: $args", app.sourceContext)
          }
        case STuple(_) =>
          // If it's a tuple then the type of the application depend on the index.
          args match {
            case Seq(Constant(index, _: SNumericType)) =>
              val fieldIndex = SByte.downcast(index.asInstanceOf[AnyVal]) + 1
              mkSelectField(new_f.asTuple, fieldIndex.toByte)
            case Seq(index) =>
              val typedIndex = assignType(env, index)
              typedIndex.tpe match {
                case _: SNumericType =>
                  mkByIndex(new_f.asCollection[SAny.type], typedIndex.upcastTo(SInt), None)
                case _ =>
                  error(s"Invalid argument type of tuple application $app: expected numeric type; actual: ${typedIndex.tpe}", typedIndex.sourceContext)
              }
            case _ =>
              error(s"Invalid argument of tuple application $app: expected integer value; actual: $args", app.sourceContext)
          }
        case t =>
          error(s"Invalid array application $app: array type is expected but was $t", app.sourceContext)
      }) match {
        case PredefinedFuncApply(irNode) => irNode
        case v => v
      }

    case mc @ MethodCallLike(obj, m, args, _) =>
      val newObj = assignType(env, obj)
      val newArgs = args.map(assignType(env, _))
      newObj.tpe match {
        case tColl: SCollectionType[a] => (m, newArgs) match {
          case ("++", Seq(r)) =>
            if (r.tpe == tColl)
              mkAppend(newObj.asCollection[a], r.asCollection[a])
            else
              error(s"Invalid argument type for $m, expected $tColl but was ${r.tpe}", r.sourceContext)
          case (SCollection(method), _) =>
            val typeSubst = method.stype match {
              case sfunc @ SFunc(_, _, _) =>
                val newArgsTypes = newArgs.map(_.tpe)
                val actualTypes = newObj.tpe +: newArgsTypes
                unifyTypeLists(sfunc.tDom, actualTypes) match {
                  case Some(subst) =>
                    val concrFunTpe = applySubst(sfunc, subst).asFunc
                    val newMethod = method.withSType(concrFunTpe)
                    val concrFunArgsTypes = concrFunTpe.tDom.tail
                    if (newArgsTypes != concrFunArgsTypes)
                      error(s"Invalid method $newMethod argument type: expected $concrFunArgsTypes; actual: $newArgsTypes", mc.sourceContext)
                    subst
                  case None =>
                    error(s"Invalid argument type of method call $mc : expected ${sfunc.tDom}; actual: $actualTypes", mc.sourceContext)
                }
              case _ => emptySubst
            }
            method.irBuilder.flatMap(_.lift(builder, newObj, method, newArgs, typeSubst))
              .getOrElse(mkMethodCall(newObj, method, newArgs, typeSubst))

          case _ =>
            throw new NonApplicableMethod(s"Unknown symbol $m, which is used as operation with arguments $newObj and $newArgs", mc.sourceContext.toOption)
        }
        case SGroupElement => (m, newArgs) match {
          case ("*", Seq(r)) =>
            if (r.tpe == SGroupElement)
              mkMultiplyGroup(newObj.asGroupElement, r.asGroupElement)
            else
              error(s"Invalid argument type for $m, expected $SGroupElement but was ${r.tpe}", r.sourceContext)
          case _ =>
            throw new NonApplicableMethod(s"Unknown symbol $m, which is used as ($newObj) $m ($newArgs)", mc.sourceContext.toOption)
        }
        case _: SNumericType => (m, newArgs) match {
          case("+" | "*" | "^" | ">>" | "<<" | ">>>", Seq(r)) => r.tpe match {
            case _: SNumericType => m match {
              case "*" => bimap(env, "*", newObj.asNumValue, r.asNumValue)(mkMultiply)(tT, tT)
              case "+" => bimap(env, "+", newObj.asNumValue, r.asNumValue)(mkPlus)(tT, tT)
              case "^" => bimap(env, "^", newObj.asNumValue, r.asNumValue)(mkBitXor)(tT, tT)
              case ">>" => bimap(env, ">>", newObj.asNumValue, r.asNumValue)(mkBitShiftRight)(tT, tT)
              case "<<" => bimap(env, "<<", newObj.asNumValue, r.asNumValue)(mkBitShiftLeft)(tT, tT)
              case ">>>" => bimap(env, ">>>", newObj.asNumValue, r.asNumValue)(mkBitShiftRightZeroed)(tT, tT)
            }
            case _ =>
              throw new InvalidBinaryOperationParameters(s"Invalid argument type for $m, expected ${newObj.tpe} but was ${r.tpe}", r.sourceContext.toOption)
          }
          case _ =>
            throw new NonApplicableMethod(s"Unknown symbol $m, which is used as ($newObj) $m ($newArgs)", mc.sourceContext.toOption)
        }

        case SSigmaProp => (m, newArgs) match {
          case ("||" | "&&", Seq(r)) => r.tpe match {
            case SBoolean =>
              val (a,b) = (Select(newObj, SSigmaProp.IsProven, Some(SBoolean)).asBoolValue, r.asBoolValue)
              val res = if (m == "||") mkBinOr(a,b) else mkBinAnd(a,b)
              res
            case SSigmaProp =>
              val (a,b) = (newObj.asSigmaProp, r.asSigmaProp)
              val res = if (m == "||") mkSigmaOr(Seq(a,b)) else mkSigmaAnd(Seq(a,b))
              res
            case _ =>
              error(s"Invalid argument type for $m, expected $SSigmaProp but was ${r.tpe}", r.sourceContext)
          }
          case _ =>
            throw new NonApplicableMethod(s"Unknown symbol $m, which is used as ($newObj) $m ($newArgs)", mc.sourceContext.toOption)
        }
        case SBoolean => (m, newArgs) match {
          case ("||" | "&&" | "^", Seq(r)) => r.tpe match {
            case SBoolean => m match {
              case "||" => mkBinOr(newObj.asBoolValue, r.asBoolValue)
              case "&&" => mkBinAnd(newObj.asBoolValue, r.asBoolValue)
              case "^" => mkBinXor(newObj.asBoolValue, r.asBoolValue)

            }
            case SSigmaProp =>
              val (a,b) = (newObj.asBoolValue, Select(r, SSigmaProp.IsProven, Some(SBoolean)).asBoolValue)
              val res = if (m == "||") mkBinOr(a,b) else mkBinAnd(a,b)
              res
            case _ =>
              error(s"Invalid argument type for $m, expected ${newObj.tpe} but was ${r.tpe}", r.sourceContext)
          }
          case _ =>
            throw new NonApplicableMethod(s"Unknown symbol $m, which is used as ($newObj) $m ($newArgs)", mc.sourceContext.toOption)
        }
        case _: SString.type => (m, newArgs) match {
          case ("+", Seq(r)) => (newObj, r) match {
            case (cl : Constant[SString.type]@unchecked, cr : Constant[SString.type]@unchecked) =>
              mkStringConcat(cl, cr)
            case _ =>
              throw new InvalidBinaryOperationParameters(s"Invalid argument type for $m, expected ${newObj.tpe} but was ${r.tpe}", r.sourceContext.toOption)
          }
          case _ =>
            throw new NonApplicableMethod(s"Unknown symbol $m, which is used as ($newObj) $m ($newArgs)", mc.sourceContext.toOption)
        }
        case t =>
          error(s"Invalid operation $mc on type $t", mc.sourceContext)
      }

    case app @ ApplyTypes(input, targs) =>
      val newInput = assignType(env, input)
      newInput.tpe match {
        case genFunTpe @ SFunc(_, _, tpeParams) =>
          if (tpeParams.lengthCompare(targs.length) != 0)
            error(s"Wrong number of type arguments $app: expected $tpeParams but provided $targs. " +
                  s"Note that partial application of type parameters is not supported.", app.sourceContext)
          val subst = tpeParams.map(_.ident).zip(targs).toMap
          val concrFunTpe = applySubst(genFunTpe, subst).asFunc
          newInput match {
            case Select(obj, n, _) => mkSelect(obj, n, Some(concrFunTpe.tRange))
            case Ident(name, _) => mkIdent(name, concrFunTpe)
          }
        case _ =>
          error(s"Invalid application of type arguments $app: function $input doesn't have type parameters", input.sourceContext)
      }

//    case app @ ApplyTypes(in, targs) =>
//      val newIn = assignType(env, in)
//      ApplyTypes(newIn, targs)
//      error(s"Invalid application of type arguments $app: expression doesn't have type parameters")

    case If(c, t, e) =>
      val c1 = assignType(env, c).asValue[SBoolean.type]
      val t1 = assignType(env, t)
      val e1 = assignType(env, e)
      val ite = mkIf(c1, t1, e1)
      if (c1.tpe != SBoolean)
        error(s"Invalid type of condition in $ite: expected Boolean; actual: ${c1.tpe}", c.sourceContext)
      if (t1.tpe != e1.tpe)
        error(s"Invalid type of condition $ite: both branches should have the same type but was ${t1.tpe} and ${e1.tpe}", t1.sourceContext)
      ite

    case op @ AND(input) =>
      val input1 = assignType(env, input).asValue[SCollection[SBoolean.type]]
      if (!(input1.tpe.isCollection && input1.tpe.elemType == SBoolean))
        error(s"Invalid operation AND: $op", op.sourceContext)
      mkAND(input1)

    case op @ OR(input) =>
      val input1 = assignType(env, input).asValue[SCollection[SBoolean.type]]
      if (!(input1.tpe.isCollection && input1.tpe.elemType == SBoolean))
        error(s"Invalid operation OR: $op", op.sourceContext)
      mkOR(input1)

    case GE(l, r) => bimap(env, ">=", l, r)(mkGE[SType])(tT, SBoolean)
    case LE(l, r) => bimap(env, "<=", l, r)(mkLE[SType])(tT, SBoolean)
    case GT(l, r) => bimap(env, ">", l, r) (mkGT[SType])(tT, SBoolean)
    case LT(l, r) => bimap(env, "<", l, r) (mkLT[SType])(tT, SBoolean)
    case EQ(l, r) => bimap2(env, "==", l, r)(mkEQ[SType])
    case NEQ(l, r) => bimap2(env, "!=", l, r)(mkNEQ[SType])

    case ArithOp(l, r, OpCodes.MinusCode) => bimap(env, "-", l.asNumValue, r.asNumValue)(mkMinus)(tT, tT)
    case ArithOp(l, r, OpCodes.PlusCode) => bimap(env, "+", l.asNumValue, r.asNumValue)(mkPlus)(tT, tT)
    case ArithOp(l, r, OpCodes.MultiplyCode) => bimap(env, "*", l.asNumValue, r.asNumValue)(mkMultiply)(tT, tT)
    case ArithOp(l, r, OpCodes.ModuloCode) => bimap(env, "%", l.asNumValue, r.asNumValue)(mkModulo)(tT, tT)
    case ArithOp(l, r, OpCodes.DivisionCode) => bimap(env, "/", l.asNumValue, r.asNumValue)(mkDivide)(tT, tT)
    case ArithOp(l, r, OpCodes.MinCode) => bimap(env, "min", l.asNumValue, r.asNumValue)(mkMin)(tT, tT)
    case ArithOp(l, r, OpCodes.MaxCode) => bimap(env, "max", l.asNumValue, r.asNumValue)(mkMax)(tT, tT)

    case BitOp(l, r, OpCodes.BitOrCode) => bimap(env, "|", l.asNumValue, r.asNumValue)(mkBitOr)(tT, tT)
    case BitOp(l, r, OpCodes.BitAndCode) => bimap(env, "&", l.asNumValue, r.asNumValue)(mkBitAnd)(tT, tT)
    case BitOp(l, r, OpCodes.BitXorCode) => bimap(env, "^", l.asNumValue, r.asNumValue)(mkBitXor)(tT, tT)

    case Xor(l, r) => bimap(env, "|", l, r)(mkXor)(SByteArray, SByteArray)
    case MultiplyGroup(l, r) => bimap(env, "*", l, r)(mkMultiplyGroup)(SGroupElement, SGroupElement)

    case Exponentiate(l, r) =>
      val l1 = assignType(env, l).asGroupElement
      val r1 = assignType(env, r).asBigInt
      if (l1.tpe != SGroupElement || r1.tpe != SBigInt)
        error(s"Invalid binary operation Exponentiate: expected argument types ($SGroupElement, $SBigInt); actual: (${l.tpe}, ${r.tpe})", l.sourceContext)
      mkExponentiate(l1, r1)

    case ByIndex(col, i, defaultValue) =>
      val c1 = assignType(env, col).asCollection[SType]
      if (!c1.tpe.isCollectionLike)
        error(s"Invalid operation ByIndex: expected Collection argument type; actual: (${col.tpe})", col.sourceContext)
      defaultValue match {
        case Some(v) if v.tpe.typeCode != c1.tpe.elemType.typeCode =>
            error(s"Invalid operation ByIndex: expected default value type (${c1.tpe.elemType}); actual: (${v.tpe})", v.sourceContext)
        case ref @ _ => mkByIndex(c1, i, ref)
      }

    case SizeOf(col) =>
      val c1 = assignType(env, col).asCollection[SType]
      if (!c1.tpe.isCollectionLike)
        error(s"Invalid operation SizeOf: expected argument types ($SCollection); actual: (${col.tpe})", col.sourceContext)
      mkSizeOf(c1)

    case SigmaPropIsProven(p) =>
      val p1 = assignType(env, p)
      if (!p1.tpe.isSigmaProp)
        error(s"Invalid operation IsValid: expected argument types ($SSigmaProp); actual: (${p.tpe})", p.sourceContext)
      SigmaPropIsProven(p1.asSigmaProp)

    case SigmaPropBytes(p) =>
      val p1 = assignType(env, p)
      if (!p1.tpe.isSigmaProp)
        error(s"Invalid operation ProofBytes: expected argument types ($SSigmaProp); actual: (${p.tpe})", p.sourceContext)
      SigmaPropBytes(p1.asSigmaProp)

    case LogicalNot(i) => unmap(env, "!", i.asBoolValue)(mkLogicalNot)(SBoolean)
    case Negation(i) => unmap[SNumericType](env, "-", i.asNumValue)(mkNegation)(tT)
    case BitInversion(i) => unmap[SNumericType](env, "~", i.asNumValue)(mkBitInversion)(tT)

    case SomeValue(x) => SomeValue(assignType(env, x))
    case v: NoneValue[_] => v

    case Global => Global
    case Context => Context
    case Height => Height
    case MinerPubkey => MinerPubkey
    case Self => Self
    case Inputs => Inputs
    case Outputs => Outputs
    case LastBlockUtxoRootHash => LastBlockUtxoRootHash
    case c @ LongConstant(i) if expected.isDefined && expected.get == SByte =>
      if (i >= 0 && i <= Byte.MaxValue) ByteConstant(i.toByte)
      else error(s"Value $i of type Long cannot be converted to Byte.", c.sourceContext)
    case v: ContextVariable[_] => v
    case v: GetVar[_] => v
    case v: OptionGet[_] => v
    case v: EvaluatedValue[_] => v
    case v: SigmaBoolean => v
    case v: Upcast[_, _] => v
    case v @ Select(_, _, Some(_)) => v
    case v =>
      error(s"Don't know how to assignType($v)", v.sourceContext)
  }).withEnsuredSrcCtx(bound.sourceContext)

  def assignConcreteCollection(cc: ConcreteCollection[SType], newItems: IndexedSeq[Value[SType]]) = {
    val types = newItems.map(_.tpe).distinct
    val tItem = if (cc.items.isEmpty) {
      if (cc.elementType == NoType)
        error(s"Undefined type of empty collection $cc", cc.sourceContext)
      cc.elementType
    } else {
      msgTypeOf(types).getOrElse(
        error(s"All element of array $cc should have the same type but found $types", cc.sourceContext))
    }
    builder.currentSrcCtx.withValue(cc.sourceContext) { mkConcreteCollection(newItems, tItem) }
  }

  def adaptSigmaPropToBoolean(items: Seq[Value[SType]], expectedTypes: Seq[SType]): Seq[Value[SType]] = {
    val res = items.zip(expectedTypes).map {
      case (cc: ConcreteCollection[SType]@unchecked, SBooleanArray) =>
        val items = adaptSigmaPropToBoolean(cc.items, Seq.fill(cc.items.length)(SBoolean))
        assignConcreteCollection(cc, items.toIndexedSeq)
      case (it, SBoolean) if it.tpe == SSigmaProp => SigmaPropIsProven(it.asSigmaProp)
      case (it,_) => it
    }
    res
  }

  def bimap[T <: SType]
      (env: Map[String, SType], op: String, l: Value[T], r: Value[T])
      (mkNode: (Value[T], Value[T]) => SValue)
      (tArg: SType, tRes: SType): SValue = {
    val l1 = assignType(env, l).asValue[T]
    val r1 = assignType(env, r).asValue[T]
    val safeMkNode = { (left: Value[T], right: Value[T]) =>
      try {
        val node = mkNode(left, right)
        if (node.tpe == NoType)
          error("No type can be assigned to expression", l.sourceContext)
        node
      } catch {
        case e: Throwable =>
          throw new InvalidBinaryOperationParameters(s"operation: $op: $e", l.sourceContext.toOption)
      }
    }
    (l1.tpe, r1.tpe) match {
      case (t1: SNumericType, t2: SNumericType) if t1 != t2 =>
        safeMkNode(l1, r1)
      case (t1, t2) =>
        val substOpt = unifyTypes(SFunc(Vector(tArg, tArg), tRes), SFunc(Vector(t1, t2), tRes))
        if (substOpt.isDefined)
          safeMkNode(l1, r1)
        else
          throw new InvalidBinaryOperationParameters(s"Invalid binary operation $op: expected argument types ($tArg, $tArg); actual: (${l1.tpe }, ${r1.tpe })", l.sourceContext.toOption)
    }

  }

  def bimap2[T <: SType]
      (env: Map[String, SType], op: String, l: Value[T], r: Value[T])
          (newNode: (Value[T], Value[T]) => SValue): SValue = {
    val l1 = assignType(env, l).asValue[T]
    val r1 = assignType(env, r).asValue[T]
    try {
      newNode(l1, r1)
    } catch {
      case e: Throwable =>
        throw new InvalidBinaryOperationParameters(s"operation $op: $e", l.sourceContext.toOption)
    }
  }

  def unmap[T <: SType](env: Map[String, SType], op: String, i: Value[T])
                       (newNode: Value[T] => SValue)
                       (tArg: SType): SValue = {
    val i1 = assignType(env, i).asValue[T]
    if (!i1.tpe.isNumType && i1.tpe != tArg)
      throw new InvalidUnaryOperationParameters(s"Invalid unary op $op: expected argument type $tArg, actual: ${i1.tpe}", i.sourceContext.toOption)
    try {
      newNode(i1)
    } catch {
      case e: Throwable =>
        throw new InvalidUnaryOperationParameters(s"operation $op error: $e", i.sourceContext.toOption)
    }
  }

  def typecheck(bound: SValue): SValue = {
    val assigned = assignType(predefinedEnv, bound)
    if (assigned.tpe == NoType)
      error(s"No type can be assigned to expression $assigned", bound.sourceContext)

    // traverse the tree bottom-up checking that all the nodes have a type
    var untyped: SValue = null
    rewrite(everywherebu(rule[SValue]{
      case v =>
        if (v.tpe == NoType) untyped = v
        v
    }))(assigned)

    if (untyped != null)
      error(s"Errors found in $bound while assigning types to expression: $untyped assigned NoType", untyped.sourceContext)

    assigned
  }
}

object SigmaTyper {

  type STypeSubst = Map[STypeIdent, SType]
  val emptySubst = Map.empty[STypeIdent, SType]

  /** Performs pairwise type unification making sure each type variable is equally
    * substituted in all items. */
  def unifyTypeLists(items1: Seq[SType], items2: Seq[SType]): Option[STypeSubst] = {
    // unify items pairwise independently
    val itemsUni = (items1, items2).zipped.map((t1, t2) => unifyTypes(t1,t2))
    if (itemsUni.forall(_.isDefined)) {
      // merge substitutions making sure the same id is equally substituted in all items
      val merged = itemsUni.foldLeft(emptySubst)((acc, subst) => {
        var res = acc
        for ((id, t) <- subst.get) {
          if (res.contains(id) && res(id) != t) return None
          res = res + (id -> t)
        }
        res
      })
      Some(merged)
    } else
      None
  }

  private val unifiedWithoutSubst = Some(emptySubst)

  /** Finds a substitution `subst` of type variables such that unifyTypes(applySubst(t1, subst), t2) shouldBe Some(emptySubst) */
  def unifyTypes(t1: SType, t2: SType): Option[STypeSubst] = (t1, t2) match {
    case (_ @ STypeIdent(n1), _ @ STypeIdent(n2)) =>
      if (n1 == n2) unifiedWithoutSubst else None
    case (id1 @ STypeIdent(_), _) =>
      Some(Map(id1 -> t2))
    case (e1: SCollectionType[_], e2: SCollectionType[_]) =>
      unifyTypes(e1.elemType, e2.elemType)
    case (e1: SCollectionType[_], _: STuple) =>
      unifyTypes(e1.elemType, SAny)
    case (e1: SOption[_], e2: SOption[_]) =>
      unifyTypes(e1.elemType, e2.elemType)
    case (e1: STuple, e2: STuple) if e1.items.length == e2.items.length =>
      unifyTypeLists(e1.items, e2.items)
    case (e1: SFunc, e2: SFunc) if e1.tDom.length == e2.tDom.length =>
      unifyTypeLists(e1.tDom :+ e1.tRange, e2.tDom :+ e2.tRange)
    case (STypeApply(name1, args1), STypeApply(name2, args2))
      if name1 == name2 && args1.length == args2.length =>
      unifyTypeLists(args1, args2)
    case (SBoolean, SSigmaProp) => // it is necessary for implicit conversion in Coll(bool, prop, bool)
      unifiedWithoutSubst
    case (SPrimType(e1), SPrimType(e2)) if e1 == e2 =>
      unifiedWithoutSubst
    case (SAny, _) =>
      unifiedWithoutSubst
    case _ => None
  }

  def applySubst(tpe: SType, subst: STypeSubst): SType = tpe match {
    case SFunc(args, res, tparams) =>
      val remainingVars = tparams.filterNot { p => subst.contains(p.ident) }
      SFunc(args.map(applySubst(_, subst)), applySubst(res, subst), remainingVars)
    case _ =>
      val substRule = rule[SType] {
        case id: STypeIdent if subst.contains(id) => subst(id)
      }
      rewrite(everywherebu(substRule))(tpe)
  }

  def msgType(t1: SType, t2: SType): Option[SType] = unifyTypes(t1, t2) match {
    case Some(_) => Some(t1)
    case None => unifyTypes(t2, t1).map(_ => t2)
  }

  /** Most Specific Generalized (MSG) type of ts.
    * Currently just the type of the first element as long as all the elements have the same type. */
  def msgTypeOf(ts: Seq[SType]): Option[SType] = {
    if (ts.isEmpty) None
    else {
      var res: SType = ts.head
      for (t <- ts.iterator.drop(1)) {
        msgType(t, res) match {
          case Some(msg) => res = msg //assign new
          case None => return None
        }
      }
      Some(res)
    }
  }

  def error(msg: String, srcCtx: Nullable[SourceContext]) = throw new TyperException(msg, srcCtx.toOption)
}
