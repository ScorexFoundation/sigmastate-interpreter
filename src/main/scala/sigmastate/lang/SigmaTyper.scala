package sigmastate.lang

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.Terms._
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
    if (types.size == 1) Some(types(0))
    else None
  }

  /**
    * Rewrite tree to typed tree.  Checks constituent names and types.  Uses
    * the env map to resolve bound variables and their types.
    */
  def assignType(env: Map[String, SType], bound: SValue): SValue = bound match {
    case Block(bs, res) =>
      var curEnv = env
      val bs1 = ArrayBuffer[Let]()
      for (Let(n, t, b) <- bs) {
        if (curEnv.contains(n)) error(s"Variable $n already defined ($n = ${curEnv(n)}")
        val b1 = assignType(curEnv, b)
        curEnv = curEnv + (n -> b1.tpe)
        bs1 += Let(n, b1.tpe, b1)
      }
      val res1 = assignType(curEnv, res)
      Block(bs1, res1)

    case Tuple(items) => Tuple(items.map(assignType(env, _)))

    case c @ ConcreteCollection(items) =>
      val newItems = items.map(assignType(env, _))
      val types = newItems.map(_.tpe).distinct
      val tItem = msgTypeOf(types).getOrElse(
        error(s"All element of array $c should have the same type but found $types"))
      ConcreteCollection(newItems)(tItem)

    case v @ Ident(n, _) =>
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
        error(s"Cannot find field '${n}' in the object $obj of Sigma type with fields ${obj.fields}")
      Select(newObj, n, Some(tRes))

    case sel @ Select(obj, n, None) =>
      val newObj = assignType(env, obj)
      newObj.tpe match {
        case s: SProduct =>
          val iField = s.fieldIndex(n)
          val tRes = if (iField != -1) {
            s.fields(iField)._2
          } else
            error(s"Cannot find field '${n}' in in the object $obj of Product type with fields ${s.fields}")
          Select(newObj, n, Some(tRes))
        case t =>
          error(s"Cannot get field '${n}' in in the object $obj of non-product type $t")
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
          val actualTypes = newArgs.map(_.tpe)
          unifyTypeLists(argTypes, actualTypes) match {
            case Some(subst) =>
              val concrFunTpe = applySubst(genFunTpe, subst)
              val newObj = assignType(env, obj)
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
      val newArgs = args.map(assignType(env, _))
      f.tpe match {
        case SFunc(argTypes, tRes, _) =>
          // If it's a pre-defined function application
          val actualTypes = newArgs.map(_.tpe)
          if (actualTypes != argTypes)
            error(s"Invalid argument type of application $app: expected $argTypes; actual: $actualTypes")
          Apply(new_f, newArgs)
        case tCol: SCollection[_] =>
          // If it's a collection then the application has type of that collection's element.
          // Only constant indices are supported so far
          args match {
            case Seq(IntConstant(i)) =>
              ByIndex[SType](new_f.asCollection, i.toInt)
            case _ =>
              error(s"Invalid argument of array application $app: expected integer constant; actual: $args")
          }
        case t =>
          error(s"Invalid array application $app: array type is expected but was $t")
      }

    case app @ ApplyTypes(sel: Select, targs) =>
      val newSel @ Select(obj, n, _) = assignType(env, sel)
      newSel.tpe match {
        case genFunTpe @ SFunc(argTypes, tRes, tyVars) =>
          if (tyVars.length != targs.length)
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

    case GE(l, r) => bimap(env, ">=", l, r)(GE)(SInt, SBoolean)
    case LE(l, r) => bimap(env, "<=", l, r)(LE)(SInt, SBoolean)
    case GT(l, r) => bimap(env, ">", l, r)(GT)(SInt, SBoolean)
    case LT(l, r) => bimap(env, "<", l, r)(LT)(SInt, SBoolean)
    case EQ(l, r) => bimap2(env, "==", l, r)(EQ[SType])((l,r) => l == r)
    case NEQ(l, r) => bimap2(env, "!=", l, r)(NEQ)((l,r) => l == r)

    case Plus(l, r) => bimap(env, "+", l, r)(Plus)(SInt, SInt)
    case Minus(l, r) => bimap(env, "-", l, r)(Minus)(SInt, SInt)
    case Xor(l, r) => bimap(env, "|", l, r)(Xor)(SByteArray, SByteArray)
    case MultiplyGroup(l, r) => bimap(env, "*", l, r)(MultiplyGroup)(SGroupElement, SGroupElement)
    case AppendBytes(l, r) => bimap(env, "++", l, r)(AppendBytes)(SByteArray, SByteArray)

    case Exponentiate(l, r) =>
      val l1 = assignType(env, l).asGroupElement
      val r1 = assignType(env, r).asBigInt
      if (l1.tpe != SGroupElement || r1.tpe != SBigInt)
        error(s"Invalid binary operation Exponentiate: expected argument types ($SGroupElement, $SBigInt); actual: (${l.tpe}, ${r.tpe})")
      Exponentiate(l1, r1)

    case ByIndex(col, i) =>
      val c1 = assignType(env, col).asCollection[SType]
      if (!c1.tpe.isCollection)
        error(s"Invalid operation ByIndex: expected argument types ($SCollection); actual: (${col.tpe})")
      ByIndex(c1, i)

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
    case v: EvaluatedValue[_] => v
    case v: SigmaBoolean => v
    case v => error(s"Don't know how to assignType($v)")
  }

  def bimap[T <: SType]
      (env: Map[String, SType], op: String, l: Value[T], r: Value[T])
      (f: (Value[T], Value[T]) => SValue)
      (tArg: SType, tRes: SType): SValue = {
    val l1 = assignType(env, l).asValue[T]
    val r1 = assignType(env, r).asValue[T]
    if ((l1.tpe == tArg) && (r1.tpe == tArg))
      f(l1, r1)
    else
      error(s"Invalid binary operation $op: expected argument types ($tArg, $tArg); actual: (${l1.tpe }, ${r1.tpe })")
  }

  def bimap2[T <: SType]
      (env: Map[String, SType], op: String, l: Value[T], r: Value[T])
          (f: (Value[T], Value[T]) => SValue)
          (check: (SType, SType) => Boolean): SValue = {
    val l1 = assignType(env, l).asValue[T]
    val r1 = assignType(env, r).asValue[T]
    if (check(l1.tpe, r1.tpe))
      f(l1, r1)
    else
      error(s"Invalid binary operation $op ($l1, $r1)")
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

  def unifyTypes(t1: SType, t2: SType): Option[STypeSubst] = (t1, t2) match {
    case (id1 @ STypeIdent(n1), id2 @ STypeIdent(n2)) =>
      if (n1 == n2) Some(Map(id1 -> t2)) else None
    case (id1 @ STypeIdent(n), _) =>
      Some(Map(id1 -> t2))
    case (e1: SCollection[_], e2: SCollection[_]) =>
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
      val remainingVars = tvars.filterNot(subst.contains(_))
      SFunc(args.map(applySubst(_, subst)), applySubst(res, subst), remainingVars)
    case _ =>
      val substRule = rule[SType] {
        case id: STypeIdent if subst.contains(id) => subst(id)
      }
      rewrite(everywherebu(substRule))(tpe)
  }

  def error(msg: String) = throw new TyperException(msg, None)
}
