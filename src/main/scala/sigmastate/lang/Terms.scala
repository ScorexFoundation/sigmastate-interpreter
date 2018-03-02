package sigmastate.lang

import sigmastate.{Value, NoType, SType, SFunc}

object Terms {

  type UValue = Value[SType]

  case class CUSTOMTYPE(name: String, fields: List[(String, SType)])

  case class Block(let: Option[Let], t: Value[SType]) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = ???
    def tpe: SType = t.tpe
  }

  case class Let(name: String, value: Block) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = ???
    def tpe: SType = value.tpe
  }

  case class Select(i: Value[SType], field: String) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = ???
    def tpe: SType = NoType
  }

  case class Ident(key: String, tpe: SType = NoType) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = ???
  }

  case class Apply(func: Value[SType], args: IndexedSeq[Value[SType]]) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = false
    lazy val tpe: SType = func.tpe match {
      case SFunc(_, r) => r
      case _ => NoType
    }
  }

  implicit def valueToBlock(t: Value[SType]): Block = Block(None, t)

  def typed[A <: SType,B <: SType](a: UValue, b: UValue)(f: (Value[A],Value[B]) => UValue): UValue =
    f(a.asInstanceOf[Value[A]], b.asInstanceOf[Value[B]])

  implicit class ValueOps(v: Value[SType]) {
    def asValue[T <: SType]: Value[T] = v.asInstanceOf[Value[T]]
  }
}
