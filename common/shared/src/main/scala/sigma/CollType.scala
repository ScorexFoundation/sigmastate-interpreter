package sigma

import scalan.RType

import scala.language.implicitConversions
import scala.reflect.ClassTag

/** Type descriptor for `Coll[A]` type. */
case class CollType[A](tItem: RType[A]) extends RType[Coll[A]] {
  override val classTag: ClassTag[Coll[A]] = ClassTag[Coll[A]](classOf[Coll[A]])

  override def name: String = s"Coll[${tItem.name}]"
}

object CollType {
  /** Conversion to underlying descriptor class.
    * Allows syntax like
    *
    * ```val tColl: RType[Coll[A]] = ...; tColl.tItem```
    *
    * where `tItem` is a method of `CollType`, but is not defined on `RType`.
    */
  implicit def downcastCollType[A](ct: RType[Coll[A]]): CollType[A] = ct.asInstanceOf[CollType[A]]
}
