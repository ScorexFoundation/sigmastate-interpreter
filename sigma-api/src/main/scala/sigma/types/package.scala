package sigma

import scalan.RType
import scala.reflect.classTag
package types {
  import scalan.{Internal, Nullable}

  import scala.reflect.ClassTag

  case class PrimValueType[T, Val](classTag: ClassTag[T], tVal: RType[Val]) extends RType[T] {
    override def name: String = tVal.name
  }
  
  object IsPrimValue {
    def unapply(pv: PrimValue[_]): Nullable[Any] = (pv match {
      case pv: PrimValue[_]  => Nullable(pv.value)
      case _ => Nullable.None
    })
  }

}

package object types {
  implicit val booleanRType: RType[Boolean] = PrimValueType[Boolean, scala.Boolean](classTag[Boolean], RType[scala.Boolean])
  implicit val byteRType: RType[Byte] = PrimValueType[Byte, scala.Byte](classTag[Byte], RType[scala.Byte])
  implicit val intRType: RType[Int] = PrimValueType[Int, scala.Int](classTag[Int], RType[scala.Int])
}
