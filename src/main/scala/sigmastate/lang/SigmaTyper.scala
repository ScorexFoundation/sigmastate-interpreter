package sigmastate.lang

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.ergoplatform._
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import SCollection.SBooleanArray
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.{InvalidBinaryOperationParameters, MethodNotFound, TyperException, NonApplicableMethod}
import sigmastate.lang.SigmaPredef._
import sigmastate.serialization.OpCodes
import sigmastate.utxo._

import scala.collection.mutable.ArrayBuffer

/**
  * Type inference and analysis for Sigma expressions.
  */
class SigmaTyper(val builder: SigmaBuilder) {
  import SigmaTyper._
  import builder._

  private val tT = STypeIdent("T") // to be used in typing rules

  private val predefFuncRegistry = new PredefinedFuncRegistry(builder)
  import predefFuncRegistry._

  private val predefinedEnv: Map[String, SType] =
    SigmaPredef.predefinedEnv.mapValues(_.tpe) ++
      predefFuncRegistry.funcs.map(f => f.name -> f.declaration.tpe).toMap

  /**
    * Rewrite tree to typed tree.  Checks constituent names and types.  Uses
    * the env map to resolve bound variables and their types.
    */
  def assignType(env: Map[String, SType],
                 bound: SValue,
                 expected: Option[SType] = None): SValue = bound match {
    case Block(bs, res) =>
      var curEnv = env
      val bs1 = ArrayBuffer[Val]()
      for (Val(n, _, b) <- bs) {
        if (curEnv.contains(n)) error(s"Variable $n already defined ($n = ${curEnv(n)}")
        val b1 = assignType(curEnv, b)
        curEnv = curEnv + (n -> b1.tpe)
        bs1 += mkVal(n, b1.tpe, b1)
      }
      val res1 = assignType(curEnv, res)
      mkBlock(bs1, res1)

    case Tuple(items) =>
      mkTuple(items.map(assignType(env, _)))

    case c @ ConcreteCollection(items, _) =>
      val newItems = items.map(assignType(env, _))
      assignConcreteCollection(c, newItems)

    case Ident(n, _) =>
      env.get(n) match {
        case Some(t) => mkIdent(n, t)
        case None => error(s"Cannot assign type for variable '$n' because it is not found in env $env")
      }

    case sel @ Select(obj: SigmaBoolean, n, None) =>
      val newObj = assignType(env, obj).asSigmaBoolean
      val iField = newObj.fields.indexWhere(_._1 == n)
      val tRes = if (iField != -1) {
        obj.fields(iField)._2
      }
      else
        error(s"Cannot find field '$n' in the object $obj of Sigma type with fields ${obj.fields}")
      mkSelect(newObj, n, Some(tRes))

    case sel @ Select(obj, n, None) =>
      val newObj = assignType(env, obj)
      newObj.tpe match {
        case s: SProduct =>
          val iField = s.methodIndex(n)
          val tRes = if (iField != -1) {
            s.methods(iField).stype
          } else
            throw new MethodNotFound(s"Cannot find method '$n' in in the object $obj of Product type with methods ${s.methods}")
          mkSelect(newObj, n, Some(tRes))
        case t =>
          error(s"Cannot get field '$n' in in the object $obj of non-product type $t")
      }

    case lam @ Lambda(tparams, args, t, body) =>
      for ((name, t) <- args)
        if (t == NoType)
          error(s"Invalid function $lam: undefined type of argument $name")
      val lambdaEnv = env ++ args
      val newBody = body.map(assignType(lambdaEnv, _))
      if (t != NoType) {
        if (newBody.isDefined && t != newBody.get.tpe)
          error(s"Invalid function $lam: resulting expression type ${newBody.get.tpe} doesn't equal declared type $t")
      }
      val res = mkGenLambda(tparams, args, newBody.fold(t)(_.tpe), newBody)
      res

    case app @ Apply(sel @ Select(obj, n, _), args) =>
      val newSel = assignType(env, sel)
      val newArgs = args.map(assignType(env, _))
      newSel.tpe match {
        case genFunTpe @ SFunc(argTypes, tRes, _) =>
          // If it's a function then the application has type of that function's return type.
          val newObj = assignType(env, obj)
          val actualTypes = newObj.tpe +: newArgs.map(_.tpe)
          unifyTypeLists(argTypes, actualTypes) match {
            case Some(subst) =>
              val concrFunTpe = applySubst(genFunTpe, subst)
              val newApply = mkApply(mkSelect(newObj, n, Some(concrFunTpe)), newArgs)
              newApply
            case None =>
              error(s"Invalid argument type of application $app: expected $argTypes; actual: $actualTypes")
          }
        case _ =>
          mkApply(newSel, args.map(assignType(env, _)))
      }

    case app @ Apply(f, args) =>
      val new_f = assignType(env, f)
      new_f.tpe match {
        case SFunc(argTypes, tRes, _) =>
          // If it's a pre-defined function application
          if (args.length != argTypes.length)
            error(s"Invalid argument type of application $app: invalid number of arguments")
          val typedArgs = args.zip(argTypes).map {
            case (arg, expectedType) => assignType(env, arg, Some(expectedType))
          }
          val adaptedTypedArgs = (new_f, typedArgs) match {
            case (AllOfFunc.sym | AnyOfFunc.sym, _) =>
              adaptSigmaPropToBoolean(typedArgs, argTypes)
            case (Ident(GetVarFunc.name, _), Seq(id: Constant[SNumericType]@unchecked)) if id.tpe.isNumType =>
                Seq(ByteConstant(SByte.downcast(id.value.asInstanceOf[AnyVal])))
            case _ => typedArgs
          }
          val actualTypes = adaptedTypedArgs.map(_.tpe)
          if (actualTypes != argTypes)
            error(s"Invalid argument type of application $app: expected $argTypes; actual after typing: $actualTypes")
          mkApply(new_f, adaptedTypedArgs.toIndexedSeq)
        case _: SCollectionType[_] =>
          // If it's a collection then the application has type of that collection's element.
          args match {
            case Seq(Constant(index, _: SNumericType)) =>
              mkByIndex[SType](new_f.asCollection, SInt.upcast(index.asInstanceOf[AnyVal]), None)
            case Seq(index) =>
              val typedIndex = assignType(env, index)
              typedIndex.tpe match {
                case _: SNumericType =>
                  mkByIndex[SType](new_f.asCollection, typedIndex.upcastTo(SInt), None)
                case _ =>
                  error(s"Invalid argument type of array application $app: expected numeric type; actual: ${typedIndex.tpe}")
              }
            case _ =>
              error(s"Invalid argument of array application $app: expected integer value; actual: $args")
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
                  error(s"Invalid argument type of tuple application $app: expected numeric type; actual: ${typedIndex.tpe}")
              }
            case _ =>
              error(s"Invalid argument of tuple application $app: expected integer value; actual: $args")
          }
        case t =>
          error(s"Invalid array application $app: array type is expected but was $t")
      }

    case mc @ MethodCallLike(obj, m, args, _) =>
      val newObj = assignType(env, obj)
      val newArgs = args.map(assignType(env, _))
      newObj.tpe match {
        case tCol: SCollectionType[a] => (m, newArgs) match {
          case ("++", Seq(r)) =>
            if (r.tpe == tCol)
              mkAppend(newObj.asCollection[a], r.asCollection[a])
            else
              error(s"Invalid argument type for $m, expected $tCol but was ${r.tpe}")
          case _ =>
            throw new NonApplicableMethod(s"Unknown symbol $m, which is used as operation with arguments $newObj and $newArgs")
        }
        case SGroupElement => (m, newArgs) match {
          case ("*", Seq(r)) =>
            if (r.tpe == SGroupElement)
              mkMultiplyGroup(newObj.asGroupElement, r.asGroupElement)
            else
              error(s"Invalid argument type for $m, expected $SGroupElement but was ${r.tpe}")
          case _ =>
            throw new NonApplicableMethod(s"Unknown symbol $m, which is used as ($newObj) $m ($newArgs)")
        }
        case _: SNumericType => (m, newArgs) match {
          case("+" | "*", Seq(r)) => r.tpe match {
            case _: SNumericType => m match {
              case "*" => bimap(env, "*", newObj.asNumValue, r.asNumValue)(mkMultiply)(tT, tT)
              case "+" => bimap(env, "+", newObj.asNumValue, r.asNumValue)(mkPlus)(tT, tT)
            }
            case _ =>
              throw new InvalidBinaryOperationParameters(s"Invalid argument type for $m, expected ${newObj.tpe} but was ${r.tpe}")
          }
          case _ =>
            throw new NonApplicableMethod(s"Unknown symbol $m, which is used as ($newObj) $m ($newArgs)")
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
              error(s"Invalid argument type for $m, expected $SSigmaProp but was ${r.tpe}")
          }
          case _ =>
            throw new NonApplicableMethod(s"Unknown symbol $m, which is used as ($newObj) $m ($newArgs)")
        }
        case SBoolean => (m, newArgs) match {
          case ("||" | "&&", Seq(r)) => r.tpe match {
            case SBoolean =>
              val res = if (m == "||") mkBinOr(newObj.asBoolValue, r.asBoolValue) else mkBinAnd(newObj.asBoolValue, r.asBoolValue)
              res
            case SSigmaProp =>
              val (a,b) = (newObj.asBoolValue, Select(r, SSigmaProp.IsProven, Some(SBoolean)).asBoolValue)
              val res = if (m == "||") mkBinOr(a,b) else mkBinAnd(a,b)
              res
            case _ =>
              error(s"Invalid argument type for $m, expected ${newObj.tpe} but was ${r.tpe}")
          }
          case _ =>
            throw new NonApplicableMethod(s"Unknown symbol $m, which is used as ($newObj) $m ($newArgs)")
        }
        case _: SString.type => (m, newArgs) match {
          case ("+", Seq(r)) => r.tpe match {
            case _: SString.type =>
              bimap(env, "+", newObj.asStringValue, r.asStringValue)(mkStringConcat)(tT, tT)
            case _ =>
              throw new InvalidBinaryOperationParameters(s"Invalid argument type for $m, expected ${newObj.tpe} but was ${r.tpe}")
          }
          case _ =>
            throw new NonApplicableMethod(s"Unknown symbol $m, which is used as ($newObj) $m ($newArgs)")
        }
        case t =>
          error(s"Invalid operation $mc on type $t")
      }

    case app @ ApplyTypes(input, targs) =>
      val newInput = assignType(env, input)
      newInput.tpe match {
        case genFunTpe @ SFunc(_, _, tpeParams) =>
          if (tpeParams.lengthCompare(targs.length) != 0)
            error(s"Wrong number of type arguments $app: expected $tpeParams but provided $targs. " +
                  s"Note that partial application of type parameters is not supported.")
          val subst = tpeParams.map(_.ident).zip(targs).toMap
          val concrFunTpe = applySubst(genFunTpe, subst).asFunc
          newInput match {
            case Select(obj, n, _) => mkSelect(obj, n, Some(concrFunTpe.tRange))
            case Ident(name, _) => mkIdent(name, concrFunTpe)
          }
        case _ =>
          error(s"Invalid application of type arguments $app: function $input doesn't have type parameters")
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
        error(s"Invalid type of condition in $ite: expected Boolean; actual: ${c1.tpe}")
      if (t1.tpe != e1.tpe)
        error(s"Invalid type of condition $ite: both branches should have the same type but was ${t1.tpe} and ${e1.tpe}")
      ite

    case op @ AND(input) =>
      val input1 = assignType(env, input).asValue[SCollection[SBoolean.type]]
      if (!(input1.tpe.isCollection && input1.tpe.elemType == SBoolean))
        error(s"Invalid operation AND: $op")
      mkAND(input1)

    case op @ OR(input) =>
      val input1 = assignType(env, input).asValue[SCollection[SBoolean.type]]
      if (!(input1.tpe.isCollection && input1.tpe.elemType == SBoolean))
        error(s"Invalid operation OR: $op")
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

    case Xor(l, r) => bimap(env, "|", l, r)(mkXor)(SByteArray, SByteArray)
    case MultiplyGroup(l, r) => bimap(env, "*", l, r)(mkMultiplyGroup)(SGroupElement, SGroupElement)

    case Exponentiate(l, r) =>
      val l1 = assignType(env, l).asGroupElement
      val r1 = assignType(env, r).asBigInt
      if (l1.tpe != SGroupElement || r1.tpe != SBigInt)
        error(s"Invalid binary operation Exponentiate: expected argument types ($SGroupElement, $SBigInt); actual: (${l.tpe}, ${r.tpe})")
      mkExponentiate(l1, r1)

    case ByIndex(col, i, defaultValue) =>
      val c1 = assignType(env, col).asCollection[SType]
      if (!c1.tpe.isCollectionLike)
        error(s"Invalid operation ByIndex: expected Collection argument type; actual: (${col.tpe})")
      defaultValue match {
        case Some(v) if v.tpe.typeCode != c1.tpe.elemType.typeCode =>
            error(s"Invalid operation ByIndex: expected default value type (${c1.tpe.elemType}); actual: (${v.tpe})")
        case ref @ _ => ByIndex(c1, i, ref)
      }

    case SizeOf(col) =>
      val c1 = assignType(env, col).asCollection[SType]
      if (!c1.tpe.isCollectionLike)
        error(s"Invalid operation SizeOf: expected argument types ($SCollection); actual: (${col.tpe})")
      mkSizeOf(c1)

    case SigmaPropIsProven(p) =>
      val p1 = assignType(env, p)
      if (!p1.tpe.isSigmaProp)
        error(s"Invalid operation IsValid: expected argument types ($SSigmaProp); actual: (${p.tpe})")
      SigmaPropIsProven(p1.asSigmaProp)

    case SigmaPropBytes(p) =>
      val p1 = assignType(env, p)
      if (!p1.tpe.isSigmaProp)
        error(s"Invalid operation ProofBytes: expected argument types ($SSigmaProp); actual: (${p.tpe})")
      SigmaPropBytes(p1.asSigmaProp)

    case SomeValue(x) => SomeValue(assignType(env, x))
    case v: NoneValue[_] => v

    case Height => Height
    case MinerPubkey => MinerPubkey
    case Self => Self
    case Inputs => Inputs
    case Outputs => Outputs
    case LastBlockUtxoRootHash => LastBlockUtxoRootHash
    case LongConstant(i) if expected.isDefined && expected.get == SByte =>
      if (i >= 0 && i <= Byte.MaxValue) ByteConstant(i.toByte)
      else error(s"Value $i of type Long cannot be converted to Byte.")
    case v: ContextVariable[_] => v
    case v: GetVar[_] => v
    case v: OptionGet[_] => v
    case v: EvaluatedValue[_] => v
    case v: SigmaBoolean => v
    case v: Upcast[_, _] => v
    case v @ Select(_, _, Some(_)) => v
    case v =>
      error(s"Don't know how to assignType($v)")
  }

  def assignConcreteCollection(cc: ConcreteCollection[SType], newItems: IndexedSeq[Value[SType]]) = {
    val types = newItems.map(_.tpe).distinct
    val tItem = if (cc.items.isEmpty) {
      if (cc.elementType == NoType)
        error(s"Undefined type of empty collection $cc")
      cc.elementType
    } else {
      msgTypeOf(types).getOrElse(
        error(s"All element of array $cc should have the same type but found $types"))
    }
    ConcreteCollection(newItems)(tItem)
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
        mkNode(left, right)
      } catch {
        case e: Throwable =>
          throw new InvalidBinaryOperationParameters(s"operation: $op: $e")
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
          throw new InvalidBinaryOperationParameters(s"Invalid binary operation $op: expected argument types ($tArg, $tArg); actual: (${l1.tpe }, ${r1.tpe })")
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
        throw new InvalidBinaryOperationParameters(s"operation $op: $e")
    }
  }

  def typecheck(bound: SValue): SValue = {
    val assigned = assignType(predefinedEnv, bound)
    if (assigned.tpe == NoType) error(s"No type can be assigned to expression $assigned")

    // traverse the tree bottom-up checking that all the nodes have a type
    var untyped: SValue = null
    rewrite(everywherebu(rule[SValue]{
      case v =>
        if (v.tpe == NoType) untyped = v
        v
    }))(assigned)

    if (untyped != null)
      error(s"Errors found in $bound while assigning types to expression: $untyped assigned NoType")

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
    case (id1 @ STypeIdent(n1), id2 @ STypeIdent(n2)) =>
      if (n1 == n2) unifiedWithoutSubst else None
    case (id1 @ STypeIdent(n), _) =>
      Some(Map(id1 -> t2))
    case (e1: SCollectionType[_], e2: SCollectionType[_]) =>
      unifyTypes(e1.elemType, e2.elemType)
    case (e1: SCollectionType[_], e2: STuple) =>
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
    case (SBoolean, SSigmaProp) =>
      unifiedWithoutSubst
    case (SPrimType(e1), SPrimType(e2)) if e1 == e2 =>
      unifiedWithoutSubst
    case (e1: SProduct, e2: SProduct) if e1.sameMethods(e2) =>
      unifyTypeLists(e1.methods.map(_.stype), e2.methods.map(_.stype))
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

  def error(msg: String) = throw new TyperException(msg, None)
}
