package sigmastate.lang

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.ergoplatform._
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes
import sigmastate.utxo._

import scala.collection.mutable.ArrayBuffer

/**
  * Analyses for typed lambda calculus expressions.  A simple free variable
  * analysis plus name and type analysis.  There are two versions of the
  * latter here: one (tipe) that constructs an explicit environment separate
  * from the AST, and one (tipe2) that represents names by references to the
  * nodes of their binding lambda expressions.
  */
class SigmaTyper {
  import SigmaTyper._

  /** Most Specific Generalized (MSG) type of ts.
    * Currently just the type of the first element as long as all the elements have the same type. */
  def msgTypeOf(ts: Seq[SType]): Option[SType] = {
    val types = ts.distinct
    if (types.isEmpty) None
    else
    if (types.lengthCompare(1) == 0) Some(types.head)
    else None
  }

  private val tT = STypeIdent("T") // to be used in typing rules

  /**
    * Rewrite tree to typed tree.  Checks constituent names and types.  Uses
    * the env map to resolve bound variables and their types.
    */
  def assignType(env: Map[String, SType],
                 bound: SValue,
                 expected: Option[SType] = None): SValue = bound match {
    case Block(bs, res) =>
      var curEnv = env
      val bs1 = ArrayBuffer[Let]()
      for (Let(n, _, b) <- bs) {
        if (curEnv.contains(n)) error(s"Variable $n already defined ($n = ${curEnv(n)}")
        val b1 = assignType(curEnv, b)
        curEnv = curEnv + (n -> b1.tpe)
        bs1 += Let(n, b1.tpe, b1)
      }
      val res1 = assignType(curEnv, res)
      Block(bs1, res1)

    case Tuple(items) => Tuple(items.map(assignType(env, _)))

    case c @ ConcreteCollection(items, _) =>
      val newItems = items.map(assignType(env, _))
      val types = newItems.map(_.tpe).distinct
      val tItem = if (items.isEmpty) {
        if (c.elementType == NoType)
          error(s"Undefined type of empty collection $c")
        c.elementType
      } else {
        msgTypeOf(types).getOrElse(
          error(s"All element of array $c should have the same type but found $types"))
      }
      ConcreteCollection(newItems)(tItem)

    case Ident(n, _) =>
      env.get(n) match {
        case Some(t) => Ident(n, t)
        case None => error(s"Cannot assign type for variable '$n' because it is not found in env $env")
      }

    case sel @ Select(obj: SigmaBoolean, n, None) =>
      val newObj = assignType(env, obj).asSigmaValue
      val iField = newObj.fields.indexWhere(_._1 == n)
      val tRes = if (iField != -1) {
        obj.fields(iField)._2
      }
      else
        error(s"Cannot find field '$n' in the object $obj of Sigma type with fields ${obj.fields}")
      Select(newObj, n, Some(tRes))

    case sel @ Select(obj, n, None) =>
      val newObj = assignType(env, obj)
      newObj.tpe match {
        case s: SProduct =>
          val iField = s.fieldIndex(n)
          val tRes = if (iField != -1) {
            s.fields(iField)._2
          } else
            error(s"Cannot find field '$n' in in the object $obj of Product type with fields ${s.fields}")
          Select(newObj, n, Some(tRes))
        case t =>
          error(s"Cannot get field '$n' in in the object $obj of non-product type $t")
      }

    case lam @ Lambda(args, t, body) =>
      for ((name, t) <- args)
        if (t == NoType)
          error(s"Invalid function $lam: undefined type of argument $name")
      val lambdaEnv = env ++ args
      val newBody = body.map(assignType(lambdaEnv, _))
      if (t != NoType) {
        if (newBody.isDefined && t != newBody.get.tpe)
          error(s"Invalid function $lam: resulting expression type ${newBody.get.tpe} doesn't equal declared type $t")
      }
      val res = Lambda(args, newBody.fold(t)(_.tpe), newBody)
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
              val newApply = Apply(Select(newObj, n, Some(concrFunTpe)), newArgs)
              newApply
            case None =>
              error(s"Invalid argument type of application $app: expected $argTypes; actual: $actualTypes")
          }
        case _ =>
          Apply(newSel, args.map(assignType(env, _)))
      }

    case app @ Apply(f, args) =>
      val new_f = assignType(env, f)

      new_f.tpe match {
        case SFunc(argTypes, tRes, _) =>
          // If it's a pre-defined function application
          if (args.length != argTypes.length)
            error(s"Invalid argument type of application $app: invalid number of arguments")
          val newArgs = args.zip(argTypes).map {
            case (arg, expectedType) => assignType(env, arg, Some(expectedType))
          }
          val actualTypes = newArgs.map(_.tpe)
          if (actualTypes != argTypes)
            error(s"Invalid argument type of application $app: expected $argTypes; actual: $actualTypes")
          Apply(new_f, newArgs)
        case SCollection(elemType) =>
          // If it's a collection then the application has type of that collection's element.
          args match {
              // todo cover all paths with tests
            case Seq(Constant(i, _: SNumericType)) =>
              ByIndex[SType](new_f.asCollection, SInt.upcast(i.asInstanceOf[AnyVal]), None)
            case Seq(Constant(i, _: SNumericType), dvConst@Constant(_, dvtpe)) =>
              if (dvtpe != elemType)
                error(s"Invalid default value type of array application $app: expected collection element type; actual: $dvtpe")
              else
                ByIndex[SType](new_f.asCollection,
                  SInt.upcast(i.asInstanceOf[AnyVal]),
                  Some(dvConst))
            case Seq(arg) =>
              val newArg = assignType(env, arg)
              newArg.tpe match {
                case _: SNumericType =>
                  ByIndex[SType](new_f.asCollection, newArg.upcastTo(SInt), None)
                case _ =>
                  error(s"Invalid argument type of array application $app: expected numeric type; actual: ${newArg.tpe}")
              }
            case Seq(i, defaultValue) =>
              val newArg = assignType(env, i)
              val typedDefValue = assignType(env, defaultValue)
              (newArg.tpe, typedDefValue.tpe) match {
                  // fixme ugly
                case (_: SNumericType, dvtpe) if dvtpe == elemType =>
                  ByIndex[SType](new_f.asCollection, newArg.upcastTo(SInt), Some(typedDefValue))
                case (_: SNumericType, _) =>
                  error(s"Invalid argument type of default value in array application $app: expected array element type; actual: ${typedDefValue.tpe}")
                  ByIndex[SType](new_f.asCollection, newArg.upcastTo(SInt), Some(typedDefValue))
                case _ =>
                  error(s"Invalid argument type of array application $app: expected numeric type; actual: ${newArg.tpe}")
              }
            case _ =>
              error(s"Invalid argument of array application $app: expected integer value; actual: $args")
          }
        case t =>
          error(s"Invalid array application $app: array type is expected but was $t")
      }

    case mc @ MethodCall(obj, m, args, _) =>
      val newObj = assignType(env, obj)
      val newArgs = args.map(assignType(env, _))
      newObj.tpe match {
        case tCol: SCollection[a] => (m, newArgs) match {
          case ("++", Seq(r)) =>
            if (r.tpe == tCol)
              Append(newObj.asCollection[a], r.asCollection[a])
            else
              error(s"Invalid argument type for $m, expected $tCol but was ${r.tpe}")
          case _ =>
            error(s"Unknown symbol $m, which is used as operation with arguments $newObj and $newArgs")
        }
        case SGroupElement => (m, newArgs) match {
          case ("*", Seq(r)) =>
            if (r.tpe == SGroupElement)
              MultiplyGroup(newObj.asGroupElement, r.asGroupElement)
            else
              error(s"Invalid argument type for $m, expected $SGroupElement but was ${r.tpe}")
          case _ =>
            error(s"Unknown symbol $m, which is used as ($newObj) $m ($newArgs)")
        }
        case nl: SNumericType => (m, newArgs) match {
          case ("*", Seq(r)) => r.tpe match {
            case nr: SNumericType =>
              bimap(env, "*", newObj.asNumValue, r.asNumValue)(Multiply)(tT, tT)
            case _ =>
              error(s"Invalid argument type for $m, expected ${newObj.tpe} but was ${r.tpe}")
          }

          case _ =>
            error(s"Unknown symbol $m, which is used as ($newObj) $m ($newArgs)")
        }
        case t =>
          error(s"Invalid operation $mc on type $t")
      }

    case app @ ApplyTypes(sel: Select, targs) =>
      val newSel @ Select(obj, n, _) = assignType(env, sel)
      newSel.tpe match {
        case genFunTpe @ SFunc(_, _, tyVars) =>
          if (tyVars.lengthCompare(targs.length) != 0)
            error(s"Wrong number of type arguments $app: expected $tyVars but provided $targs")
          val subst = tyVars.zip(targs).toMap
          val concrFunTpe = applySubst(genFunTpe, subst).asFunc
          Select(obj, n, Some(concrFunTpe.tRange))
        case _ =>
          error(s"Invalid application of type arguments $app: function $sel doesn't have type parameters")
      }

    case app @ ApplyTypes(in, targs) =>
      error(s"Invalid application of type arguments $app: expression doesn't have type parameters")

    case If(c, t, e) =>
      val c1 = assignType(env, c).asValue[SBoolean.type]
      val t1 = assignType(env, t)
      val e1 = assignType(env, e)
      val ite = If(c1, t1, e1)
      if (c1.tpe != SBoolean)
        error(s"Invalid type of condition in $ite: expected Boolean; actual: ${c1.tpe}")
      if (t1.tpe != e1.tpe)
        error(s"Invalid type of condition $ite: both branches should have the same type but was ${t1.tpe} and ${e1.tpe}")
      ite

    case op @ AND(input) =>
      val input1 = assignType(env, input).asValue[SCollection[SBoolean.type]]
      if (!(input1.tpe.isCollection && input1.tpe.elemType == SBoolean))
        error(s"Invalid operation AND: $op")
      AND(input1)

    case op @ OR(input) =>
      val input1 = assignType(env, input).asValue[SCollection[SBoolean.type]]
      if (!(input1.tpe.isCollection && input1.tpe.elemType == SBoolean))
        error(s"Invalid operation OR: $op")
      OR(input1)

    case GE(l, r) => bimap(env, ">=", l, r)(GE[SType])(tT, SBoolean)
    case LE(l, r) => bimap(env, "<=", l, r)(LE[SType])(tT, SBoolean)
    case GT(l, r) => bimap(env, ">", l, r) (GT[SType])(tT, SBoolean)
    case LT(l, r) => bimap(env, "<", l, r) (LT[SType])(tT, SBoolean)
    case EQ(l, r) => bimap2(env, "==", l, r)(EQ[SType])
    case NEQ(l, r) => bimap2(env, "!=", l, r)(NEQ[SType])

    case ArithOp(l, r, OpCodes.MinusCode) => bimap(env, "-", l.asNumValue, r.asNumValue)(Minus)(tT, tT)
    case ArithOp(l, r, OpCodes.PlusCode) => bimap(env, "+", l.asNumValue, r.asNumValue)(Plus)(tT, tT)
    case ArithOp(l, r, OpCodes.MultiplyCode) => bimap(env, "*", l.asNumValue, r.asNumValue)(Multiply)(tT, tT)
    case ArithOp(l, r, OpCodes.ModuloCode) => bimap(env, "%", l.asNumValue, r.asNumValue)(Modulo)(tT, tT)
    case ArithOp(l, r, OpCodes.DivisionCode) => bimap(env, "/", l.asNumValue, r.asNumValue)(Divide)(tT, tT)
    
    case Xor(l, r) => bimap(env, "|", l, r)(Xor)(SByteArray, SByteArray)
    case MultiplyGroup(l, r) => bimap(env, "*", l, r)(MultiplyGroup)(SGroupElement, SGroupElement)

    case Exponentiate(l, r) =>
      val l1 = assignType(env, l).asGroupElement
      val r1 = assignType(env, r).asBigInt
      if (l1.tpe != SGroupElement || r1.tpe != SBigInt)
        error(s"Invalid binary operation Exponentiate: expected argument types ($SGroupElement, $SBigInt); actual: (${l.tpe}, ${r.tpe})")
      Exponentiate(l1, r1)

    case ByIndex(col, i, defaultValue) =>
      val c1 = assignType(env, col).asCollection[SType]
      if (!c1.tpe.isCollection)
        error(s"Invalid operation ByIndex: expected argument types ($SCollection); actual: (${col.tpe})")
      defaultValue match {
        case Some(v) if v.tpe.typeCode != c1.tpe.elemType.typeCode =>
            error(s"Invalid operation ByIndex: expected default value type (${c1.tpe.elemType}); actual: (${v.tpe})")
        case ref @ _ => ByIndex(c1, i, ref)
      }

    case SizeOf(col) =>
      val c1 = assignType(env, col).asCollection[SType]
      if (!c1.tpe.isCollection)
        error(s"Invalid operation SizeOf: expected argument types ($SCollection); actual: (${col.tpe})")
      SizeOf(c1)
    
    case Height => Height
    case Self => Self
    case Inputs => Inputs
    case Outputs => Outputs
    case LastBlockUtxoRootHash => LastBlockUtxoRootHash
    case LongConstant(i) if expected.isDefined && expected.get == SByte =>
      if (i >= 0 && i <= Byte.MaxValue) ByteConstant(i.toByte)
      else error(s"Value $i of type Long cannot be converted to Byte.")
    case v: ContextVariable[_] => v
    case v: EvaluatedValue[_] => v
    case v: SigmaBoolean => v
    case v => error(s"Don't know how to assignType($v)")
  }

  def bimap[T <: SType]
      (env: Map[String, SType], op: String, l: Value[T], r: Value[T])
      (mkNode: (Value[T], Value[T]) => SValue)
      (tArg: SType, tRes: SType): SValue = {
    val l1 = assignType(env, l).asValue[T]
    val r1 = assignType(env, r).asValue[T]
    (l1.tpe, r1.tpe) match {
      case (t1: SNumericType, t2: SNumericType) if t1 != t2 =>
        val tmax = t1 max t2
        val l = l1.upcastTo(tmax)
        val r = r1.upcastTo(tmax)
        mkNode(l.asValue[T], r.asValue[T])
      case (t1, t2) =>
        val substOpt = unifyTypes(SFunc(Vector(tArg, tArg), tRes), SFunc(Vector(t1, t2), tRes))
        if (substOpt.isDefined)
          mkNode(l1, r1)
        else
          error(s"Invalid binary operation $op: expected argument types ($tArg, $tArg); actual: (${l1.tpe }, ${r1.tpe })")
    }

  }

  def bimap2[T <: SType]
      (env: Map[String, SType], op: String, l: Value[T], r: Value[T])
          (newNode: (Value[T], Value[T]) => SValue): SValue = {
    val l1 = assignType(env, l).asValue[T]
    val r1 = assignType(env, r).asValue[T]
    (l1.tpe, r1.tpe) match {
      case (t1: SNumericType, t2: SNumericType) if t1 != t2 =>
        val tmax = t1 max t2
        val l = l1.upcastTo(tmax)
        val r = r1.upcastTo(tmax)
        newNode(l.asValue[T], r.asValue[T])
      case (t1, t2) =>
        if (t1 == t2)
          newNode(l1, r1)
        else
          error(s"Invalid binary operation $op ($l1, $r1): type mismatch $t1 != $t2")
    }
  }

  def typecheck(bound: SValue): SValue = {
    val assigned = assignType(SigmaPredef.predefinedEnv.mapValues(_.tpe), bound)
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

  /** Finds a substitution `subst` of type variables such that unifyTypes(applySubst(t1, subst), t2) shouldBe Some(emptySubst) */
  def unifyTypes(t1: SType, t2: SType): Option[STypeSubst] = (t1, t2) match {
    case (id1 @ STypeIdent(n1), id2 @ STypeIdent(n2)) =>
      if (n1 == n2) Some(emptySubst) else None
    case (id1 @ STypeIdent(n), _) =>
      Some(Map(id1 -> t2))
    case (e1: SCollection[_], e2: SCollection[_]) =>
      unifyTypes(e1.elemType, e2.elemType)
    case (e1: SOption[_], e2: SOption[_]) =>
      unifyTypes(e1.elemType, e2.elemType)
    case (e1: STuple, e2: STuple) if e1.items.length == e2.items.length =>
      unifyTypeLists(e1.items, e2.items)
    case (e1: SFunc, e2: SFunc) if e1.tDom.length == e2.tDom.length =>
      unifyTypeLists(e1.tDom :+ e1.tRange, e2.tDom :+ e2.tRange)
    case (STypeApply(name1, args1), STypeApply(name2, args2))
      if name1 == name2 && args1.length == args2.length =>
      unifyTypeLists(args1, args2)
    case (SPrimType(e1), SPrimType(e2)) if e1 == e2 =>
      Some(Map())
    case (e1: SProduct, e2: SProduct) if e1.sameFields(e2) =>
      unifyTypeLists(e1.fields.map(_._2), e2.fields.map(_._2))
    case _ => None
  }

  def applySubst(tpe: SType, subst: STypeSubst): SType = tpe match {
    case SFunc(args, res, tvars) =>
      val remainingVars = tvars.filterNot(subst.contains)
      SFunc(args.map(applySubst(_, subst)), applySubst(res, subst), remainingVars)
    case _ =>
      val substRule = rule[SType] {
        case id: STypeIdent if subst.contains(id) => subst(id)
      }
      rewrite(everywherebu(substRule))(tpe)
  }

  def error(msg: String) = throw new TyperException(msg, None)
}
