package special.collection

import scalan.RType
import scala.reflect.ClassTag

/** Type descriptor for `Coll[A]` type. */
case class CollType[A](tItem: RType[A]) extends RType[Coll[A]] {
  override val classTag: ClassTag[Coll[A]] = ClassTag[Coll[A]](classOf[Coll[A]])

  override def name: String = s"Coll[${tItem.name}]"
}
