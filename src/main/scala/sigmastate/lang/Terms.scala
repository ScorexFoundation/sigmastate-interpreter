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
  case class ApplyTypes(input: Value[SType], tpeArgs: Seq[SType]) extends Value[SType] { node =>

    override val opCode: OpCode = OpCodes.Undefined

    override def cost[C <: Context[C]](context: C): Long = ???

    override def evaluated: Boolean = false
    lazy val tpe: SType = input.tpe match {
      case funcType: SFunc =>
        require(funcType.tpeParams.length == tpeArgs.length, s"Invalid number of type parameters in $node")
        val subst = funcType.tpeParams.map(_.ident).zip(tpeArgs).toMap
        SigmaTyper.applySubst(input.tpe, subst)
      case _ => input.tpe
    }
  }

  case class MethodCall(obj: Value[SType], name: String, args: IndexedSeq[Value[SType]], tpe: SType = NoType) extends Value[SType] {

    override val opCode: OpCode = OpCodes.Undefined

    override def cost[C <: Context[C]](context: C): Long = ???

    override def evaluated: Boolean = false
  }

  case class STypeParam(ident: STypeIdent, upperBound: Option[SType] = None, lowerBound: Option[SType] = None) {
    assert(upperBound.isEmpty && lowerBound.isEmpty, s"Type parameters with bounds are not supported, but found $this")
    override def toString = ident.toString + upperBound.fold("")(u => s" <: $u") + lowerBound.fold("")(l => s" >: $l")
  }
  
  case class Lambda(
        tpeParams: Seq[STypeParam],
        args: IndexedSeq[(String,SType)],
        givenResType: SType,
        body: Option[Value[SType]]) extends Value[SFunc]
  {
    require(!(tpeParams.nonEmpty && body.nonEmpty), s"Generic function definitions are not supported, but found $this")
    override val opCode: OpCode = OpCodes.Undefined
    override def cost[C <: Context[C]](context: C): Long = ???
    override def evaluated: Boolean = false
    lazy val tpe: SFunc = SFunc(args.map(_._2), givenResType ?: body.fold(NoType: SType)(_.tpe), tpeParams)
  }
  object Lambda {
    def apply(args: IndexedSeq[(String,SType)], resTpe: SType, body: Value[SType]): Lambda =
      Lambda(Nil, args, resTpe, Some(body))
    def apply(args: IndexedSeq[(String,SType)], body: Value[SType]): Lambda = Lambda(Nil, args, NoType, Some(body))
  }

  implicit class ValueOps(v: Value[SType]) {
    def asValue[T <: SType]: Value[T] = v.asInstanceOf[Value[T]]
    def asNumValue: Value[SNumericType] = v.asInstanceOf[Value[SNumericType]]
    def asStringValue: Value[SString.type] = v.asInstanceOf[Value[SString.type]]
    def asBoolValue: Value[SBoolean.type] = v.asInstanceOf[Value[SBoolean.type]]
    def asIntValue: Value[SInt.type] = v.asInstanceOf[Value[SInt.type]]
    def asLongValue: Value[SLong.type] = v.asInstanceOf[Value[SLong.type]]
    def asSigmaValue: SigmaBoolean = v.asInstanceOf[SigmaBoolean]
    def asBox: Value[SBox.type] = v.asInstanceOf[Value[SBox.type]]
    def asGroupElement: Value[SGroupElement.type] = v.asInstanceOf[Value[SGroupElement.type]]
    def asSigmaProp: Value[SSigmaProp.type] = v.asInstanceOf[Value[SSigmaProp.type]]
    def asByteArray: Value[SByteArray] = v.asInstanceOf[Value[SByteArray]]
    def asBigInt: Value[SBigInt.type] = v.asInstanceOf[Value[SBigInt.type]]
    def asCollection[T <: SType]: Value[SCollection[T]] = v.asInstanceOf[Value[SCollection[T]]]
    def asTuple: Value[STuple] = v.asInstanceOf[Value[STuple]]
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
  }
}
