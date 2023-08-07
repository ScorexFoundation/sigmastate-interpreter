package special

import scala.language.implicitConversions
import scalan.RType

import scala.reflect.{classTag, ClassTag}

package collection {
  /** Type descriptor for `Coll[A]` type. */
  case class CollType[A](tItem: RType[A]) extends RType[Coll[A]] {
    override val classTag: ClassTag[Coll[A]] = ClassTag[Coll[A]](classOf[Coll[A]])
    override def name: String = s"Coll[${tItem.name}]"
  }
}

package object collection {
  /** Forces reflection data initialization */
  val reflection = CoreLibReflection

  /** Implicit resolution of `Coll[A]` type descriptor, given a descriptor of `A`. */
  implicit def collRType[A](implicit tA: RType[A]): RType[Coll[A]] = CollType[A](tA)

  implicit val collBuilderRType: RType[CollBuilder] = RType.fromClassTag(classTag[CollBuilder])
}
