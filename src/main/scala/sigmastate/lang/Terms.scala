package sigmastate.lang

import sigmastate.Values._
import sigmastate.lang.SigmaTyper.STypeSubst
import sigmastate.utils.Overloading.Overload1
import sigmastate._

object Terms {

  case class Block(bindings: Seq[Let], result: SValue) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = false
    def tpe: SType = result.tpe
  }
  object Block {
    def apply(let: Let, result: SValue)(implicit o1: Overload1): Block = Block(Seq(let), result)
  }

  case class Let(name: String, givenType: SType, body: SValue) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = ???
    def tpe: SType = givenType ?: body.tpe
  }
  object Let {
    def apply(name: String, value: SValue): Let = Let(name, NoType, value)
  }

  case class Select(obj: Value[SType], field: String) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = ???
    val tpe: SType = obj.tpe match {
      case p: SProduct =>
        val i = p.fieldIndex(field)
        if (i == -1) NoType
        else p.fields(i)._2
      case _ => NoType
    }
  }

  case class SelectGen(obj: Value[SType], field: String, tpe: SType) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = ???
//    val tpe: SType = obj.tpe match {
//      case p: SProduct =>
//        val i = p.fieldIndex(field)
//        if (i == -1) NoType
//        else SigmaTyper.applySubst(p.fields(i)._2, tpeArgs)
//      case _ => NoType
//    }
  }

  case class Ident(name: String, tpe: SType = NoType) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = ???
  }
  object Ident {
    def apply(name: String): Ident = Ident(name, NoType)
  }

  case class Apply(func: Value[SType], args: IndexedSeq[Value[SType]]) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = false
    lazy val tpe: SType = func.tpe match {
      case SFunc(_, r) => r
      case tCol: SCollection[_] => tCol.elemType
      case _ => NoType
    }
  }

  /** Apply types for type parameters of input value. */
  case class ApplyTypes(input: Value[SType], tpeArgs: STypeSubst) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = false
    lazy val tpe: SType = SigmaTyper.applySubst(input.tpe, tpeArgs)
  }

  case class MethodCall(obj: Value[SType], name: String, args: IndexedSeq[Value[SType]], tpe: SType = NoType) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = false
  }

  case class Lambda(args: IndexedSeq[(String,SType)], givenResType: SType, body: Option[Value[SType]]) extends Value[SFunc] {
    override def cost: Int = ???
    override def evaluated: Boolean = false
    lazy val tpe: SFunc = SFunc(args.map(_._2), givenResType ?: body.fold(NoType: SType)(_.tpe))
  }
  object Lambda {
    def apply(args: IndexedSeq[(String,SType)], resTpe: SType, body: Value[SType]): Lambda =
      Lambda(args, resTpe, Some(body))
    def apply(args: IndexedSeq[(String,SType)], body: Value[SType]): Lambda = Lambda(args, NoType, Some(body))
  }

  implicit class ValueOps(v: Value[SType]) {
    def asValue[T <: SType]: Value[T] = v.asInstanceOf[Value[T]]
    def asBoolValue: Value[SBoolean.type] = v.asInstanceOf[Value[SBoolean.type]]
    def asGroupElement: Value[SGroupElement.type] = v.asInstanceOf[Value[SGroupElement.type]]
    def asByteArray: Value[SByteArray.type] = v.asInstanceOf[Value[SByteArray.type]]
    def asBigInt: Value[SBigInt.type] = v.asInstanceOf[Value[SBigInt.type]]
    def asCollection[T <: SType]: Value[SCollection[T]] = v.asInstanceOf[Value[SCollection[T]]]
  }

}
