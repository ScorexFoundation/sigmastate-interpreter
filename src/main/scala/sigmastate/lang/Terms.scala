package sigmastate.lang

import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate.utils.Overloading.Overload1
import sigmastate._
import sigmastate.serialization.OpCodes
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.interpreter.Context
import sigmastate.lang.TransformingSigmaBuilder._
import sigmastate.utxo.CostTable.Cost

object Terms {

  case class Block(bindings: Seq[Let], result: SValue) extends Value[SType] {
    override val opCode: OpCode = OpCodes.Undefined

    override def cost[C <: Context[C]](context: C): Long = ???

    override def evaluated: Boolean = false
    def tpe: SType = result.tpe
  }
  object Block {
    def apply(let: Let, result: SValue)(implicit o1: Overload1): Block =
      Block(Seq(let), result)
  }

  trait Let extends Value[SType] {
    val name: String
    val givenType: SType
    val body: SValue
  }

  case class LetNode(name: String, givenType: SType, body: SValue) extends Let {
    override val opCode: OpCode = OpCodes.Undefined

    override def cost[C <: Context[C]](context: C): Long = ???

    override def evaluated: Boolean = ???
    def tpe: SType = givenType ?: body.tpe
  }
  object Let {
    def apply(name: String, body: SValue): Let = LetNode(name, NoType, body)
    def apply(name: String, givenType: SType, body: SValue): Let = LetNode(name, givenType, body)
    def unapply(v: SValue): Option[(String, SType, SValue)] = v match {
      case LetNode(name, givenType, body) => Some((name, givenType, body))
      case _ => None
    }
  }

  case class Select(obj: Value[SType], field: String, resType: Option[SType] = None) extends Value[SType] {
    override val opCode: OpCode = OpCodes.Undefined

    override def cost[C <: Context[C]](context: C): Long = obj.cost(context) + Cost.SelectFieldDeclaration

    override def evaluated: Boolean = ???
    val tpe: SType = resType.getOrElse(obj.tpe match {
      case p: SProduct =>
        val i = p.methodIndex(field)
        if (i == -1) NoType
        else p.methods(i).stype
      case _ => NoType
    })
  }

  case class Ident(name: String, tpe: SType = NoType) extends Value[SType] {
    override val opCode: OpCode = OpCodes.Undefined

    override def cost[C <: Context[C]](context: C): Long = ???

    override def evaluated: Boolean = ???
  }
  object Ident {
    def apply(name: String): Ident = Ident(name, NoType)
  }

  case class Apply(func: Value[SType], args: IndexedSeq[Value[SType]]) extends Value[SType] {
    override val opCode: OpCode = OpCodes.Undefined

    override def cost[C <: Context[C]](context: C): Long = ???

    override def evaluated: Boolean = false
    lazy val tpe: SType = func.tpe match {
      case SFunc(_, r, _) => r
      case tCol: SCollectionType[_] => tCol.elemType
      case _ => NoType
    }
  }

  /** Apply types for type parameters of input value. */
  case class ApplyTypes(input: Value[SType], tpeArgs: Seq[SType]) extends Value[SType] {

    override val opCode: OpCode = OpCodes.Undefined

    override def cost[C <: Context[C]](context: C): Long = ???

    override def evaluated: Boolean = false
    lazy val tpe: SType = input.tpe match {
      case funcType: SFunc =>
        val subst = funcType.tpeArgs.zip(tpeArgs).toMap
        SigmaTyper.applySubst(input.tpe, subst)
      case _ => input.tpe
    }
  }

  case class MethodCall(obj: Value[SType], name: String, args: IndexedSeq[Value[SType]], tpe: SType = NoType) extends Value[SType] {

    override val opCode: OpCode = OpCodes.Undefined

    override def cost[C <: Context[C]](context: C): Long = ???

    override def evaluated: Boolean = false
  }

  case class Lambda(args: IndexedSeq[(String,SType)], givenResType: SType, body: Option[Value[SType]]) extends Value[SFunc] {

    override val opCode: OpCode = OpCodes.Undefined


    override def cost[C <: Context[C]](context: C): Long = ???

    override def evaluated: Boolean = false
    lazy val tpe: SFunc = SFunc(args.map(_._2), givenResType ?: body.fold(NoType: SType)(_.tpe))
  }
  object Lambda {
    def apply(args: IndexedSeq[(String,SType)], resTpe: SType, body: Value[SType]): Lambda =
      Lambda(args, resTpe, Some(body))
    def apply(args: IndexedSeq[(String,SType)], body: Value[SType]): Lambda = Lambda(args, NoType, Some(body))
  }

  case class OperationId(name: String, opType: SFunc)

  implicit class ValueOps(v: Value[SType]) {
    def asValue[T <: SType]: Value[T] = v.asInstanceOf[Value[T]]
    def asNumValue: Value[SNumericType] = v.asInstanceOf[Value[SNumericType]]
    def asStringValue: Value[SString.type] = v.asInstanceOf[Value[SString.type]]
    def asBoolValue: Value[SBoolean.type] = v.asInstanceOf[Value[SBoolean.type]]
    def asIntValue: Value[SInt.type] = v.asInstanceOf[Value[SInt.type]]
    def asLongValue: Value[SLong.type] = v.asInstanceOf[Value[SLong.type]]
    def asSigmaBoolean: SigmaBoolean = v.asInstanceOf[SigmaBoolean]
    def asBox: Value[SBox.type] = v.asInstanceOf[Value[SBox.type]]
    def asGroupElement: Value[SGroupElement.type] = v.asInstanceOf[Value[SGroupElement.type]]
    def asSigmaProp: Value[SSigmaProp.type] = v.asInstanceOf[Value[SSigmaProp.type]]
    def asByteArray: Value[SByteArray] = v.asInstanceOf[Value[SByteArray]]
    def asBigInt: Value[SBigInt.type] = v.asInstanceOf[Value[SBigInt.type]]
    def asCollection[T <: SType]: Value[SCollection[T]] = v.asInstanceOf[Value[SCollection[T]]]
    def asTuple: Value[STuple] = v.asInstanceOf[Value[STuple]]
    def asFunc: Value[SFunc] = v.asInstanceOf[Value[SFunc]]
    def asConcreteCollection[T <: SType]: ConcreteCollection[T] = v.asInstanceOf[ConcreteCollection[T]]
    def upcastTo[T <: SNumericType](targetType: T): Value[T] = {
      assert(v.tpe.isInstanceOf[SNumericType],
        s"Cannot upcast value of type ${v.tpe} to $targetType: only numeric types can be upcasted.")
      val tV = v.asValue[SNumericType]
      assert(targetType.max(tV.tpe) == targetType,
        s"Invalid upcast from $tV to $targetType: target type should be larger than source type.")
      if (targetType == tV.tpe) v.asValue[T]
      else
        mkUpcast(tV, targetType)
    }

    /** Every value represents an operation and that operation can be associated with a function type,
      * describing functional meaning of the operation, kind of operation signature.
      * Thus we can obtain global operation identifiers by combining Value.opName with Value.opType,
      * so that if (v1.opName == v2.opName) && (v1.opType == v2.opType) then v1 and v2 are functionally
      * point-wise equivalent.
      * This in particular means that if two _different_ ops have the same opType they _should_ have
      * different opNames.
      * Thus defined op ids are used in a Cost Model - a table of all existing primitives coupled with
      * performance parameters.
      * */
    def opType: SFunc = v match {
      case ev: EvaluatedValue[_] => SFunc(Vector(SUnit), ev.tpe)
      case Select(obj, name, Some(tres)) => SFunc(obj.tpe, tres)
      case MethodCall(obj, name, args, tres) => SFunc(obj.tpe +: args.map(_.tpe), tres)
      case Ident(name, tpe) => SFunc(Vector(SUnit), tpe)
      case t: Triple[_,_,_] => SFunc(Vector(t.left.tpe, t.right.tpe), t.tpe)
      case _ => sys.error(s"Value.opType is not defined for $v")
    }
  }
}
