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

  case class Comma(l: Value[SType], r: Value[SType]) extends Value[SType] {
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

  case class Lambda(args: IndexedSeq[(String,SType)], body: Value[SType]) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = true
    lazy val tpe: SType = SFunc(args.map(_._2), body.tpe)
  }

  implicit def valueToBlock(t: Value[SType]): Block = Block(None, t)

  def typed[A <: SType,B <: SType](a: UValue, b: UValue)(f: (Value[A],Value[B]) => UValue): UValue =
    f(a.asInstanceOf[Value[A]], b.asInstanceOf[Value[B]])

  implicit class ValueOps(v: Value[SType]) {
    def asValue[T <: SType]: Value[T] = v.asInstanceOf[Value[T]]
  }

  private[lang] def flattenComma(x: Value[SType]): List[Value[SType]] = x match {
    case Comma(l, r) => flattenComma(l) ::: flattenComma(r)
    case _ => List(x)
  }
}
