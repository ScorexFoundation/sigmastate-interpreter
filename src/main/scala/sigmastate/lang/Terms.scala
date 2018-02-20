package sigmastate.lang

import sigmastate.{SType, Value}

object Terms {

  type UValue = Value[SType]

  case class CUSTOMTYPE(name: String, fields: List[(String, SType)])

//  sealed trait LazyVal {
//    val tpe: SType
//    val value: Coeval[tpe.WrappedType]
//  }
//
//  object LazyVal {
//    private case class LazyValImpl(tpe: Type, v: Coeval[Any]) extends LazyVal {
//      override val value: Coeval[tpe.Underlying] = v.map(_.asInstanceOf[tpe.Underlying])
//    }
//
//    def apply(t: Type)(v: Coeval[t.Underlying]): LazyVal = LazyValImpl(t, v)
//  }

//  case class OBJECT(fields: Map[String, LazyVal])

  case class Block(let: Option[LET], t: Value[SType]) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = ???
  }

  case class LET(name: String, value: Block) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = ???
  }

  case class GETTER(i: Block, field: String) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = ???
  }

  case class REF(key: String) extends Value[SType] {
    override def cost: Int = ???
    override def evaluated: Boolean = ???
  }

//  case class SIG_VERIFY(message: Block, signature: Block, publicKey: Block) extends Expr { val predefinedType: Option[Type] = Some(BOOLEAN)         }
//  case class IS_DEFINED(t: Block)                                           extends Expr { val predefinedType: Option[Type] = Some(BOOLEAN)         }
//  case class GET(t: Block)                                                  extends Expr { val predefinedType: Option[Type] = None                  }
//  case object NONE                                                          extends Expr { val predefinedType: Option[Type] = Some(OPTION(NOTHING)) }
//  case class SOME(t: Block)                                                 extends Expr { val predefinedType: Option[Type] = None                  }

  implicit def valueToBlock(t: Value[SType]): Block = Block(None, t)

  def typed[A <: SType,B <: SType](a: UValue, b: UValue)(f: (Value[A],Value[B]) => UValue): UValue =
    f(a.asInstanceOf[Value[A]], b.asInstanceOf[Value[B]])

  implicit class ValueOps(v: Value[SType]) {
    def asValue[T <: SType]: Value[T] = v.asInstanceOf[Value[T]]
  }
}
