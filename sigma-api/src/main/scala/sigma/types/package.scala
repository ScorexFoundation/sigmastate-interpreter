package sigma

import scalan.RType
import scala.reflect.classTag
package types {
  import scalan.{Internal, Nullable}

  import scala.reflect.ClassTag

  trait ViewType[T, Val] extends RType[T] {
    def tVal: RType[Val]
  }

  case class PrimViewType[T, Val](classTag: ClassTag[T], tVal: RType[Val]) extends ViewType[T, Val] {
    override def name: String = tVal.name
  }

  object IsPrimView {
    def unapply(pv: PrimView[_]): Nullable[Any] = (pv match {
      case pv: PrimView[_]  => Nullable(pv.value)
      case _ => Nullable.None
    })
  }

}

package object types {
  implicit val booleanRType: RType[Boolean] = PrimViewType[Boolean, scala.Boolean](classTag[Boolean], RType[scala.Boolean])
  implicit val byteRType: RType[Byte] = PrimViewType[Byte, scala.Byte](classTag[Byte], RType[scala.Byte])
  implicit val intRType: RType[Int] = PrimViewType[Int, scala.Int](classTag[Int], RType[scala.Int])
}
