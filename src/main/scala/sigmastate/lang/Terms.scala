package sigmastate.lang

import sigmastate.{Value, NoType, SType, SFunc}

object Terms {

  type SValue = Value[SType]

  case class CUSTOMTYPE(name: String, fields: List[(String, SType)])

  case class Block(let: Option[SValue], t: SValue) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = ???
    def tpe: SType = t.tpe
  }
  object Block {
    def apply(let: Let, t: Value[SType]): Block = Block(Some(let), t)
  }

  case class Let(name: String, givenType: Option[SType], value: Block) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = ???
    def tpe: SType = givenType.getOrElse(value.tpe)
  }
  object Let {
    def apply(name: String, value: Block): Let = Let(name, None, value)
    def apply(name: String, tpe: SType, value: Block): Let = Let(name, Some(tpe), value)
  }

  case class Select(i: Value[SType], field: String) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = ???
    def tpe: SType = NoType
  }

  case class Ident(nameParts: Seq[String], tpe: SType = NoType) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = ???
  }
  object Ident {
    def apply(name: String): Ident = Ident(IndexedSeq(name), NoType)
  }

  case class Apply(func: Value[SType], args: IndexedSeq[Value[SType]]) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = false
    lazy val tpe: SType = func.tpe match {
      case SFunc(_, r) => r
      case _ => NoType
    }
  }

  case class MethodCall(obj: Value[SType], name: String, args: IndexedSeq[Value[SType]], tpe: SType = NoType) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = false
  }

  case class Lambda(args: IndexedSeq[(String,SType)], givenResType: SType, body: Option[Value[SType]]) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = true
    lazy val tpe: SType = SFunc(args.map(_._2), givenResType ?: body.fold(NoType: SType)(_.tpe))
  }
  object Lambda {
    def apply(args: IndexedSeq[(String,SType)], resTpe: SType, body: Value[SType]): Lambda =
      Lambda(args, resTpe, Some(body))
    def apply(args: IndexedSeq[(String,SType)], body: Value[SType]): Lambda = Lambda(args, NoType, Some(body))
  }

  implicit def valueToBlock(t: Value[SType]): Block = Block(None, t)

  implicit class ValueOps(v: Value[SType]) {
    def asValue[T <: SType]: Value[T] = v.asInstanceOf[Value[T]]
  }

}
