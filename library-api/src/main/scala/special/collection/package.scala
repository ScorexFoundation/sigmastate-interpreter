package special

import scala.language.implicitConversions
import scalan.RType
import scala.reflect.{ClassTag, classTag}

package collection {
  case class CollType[A](tItem: RType[A]) extends RType[Coll[A]] {
    val classTag: ClassTag[Coll[A]] = ClassTag[Coll[A]](classOf[Coll[A]])
    override def name: String = s"Coll[${tItem.name}]"
    override def isConstantSize: Boolean = false
  }
  case class ReplCollType[A](tItem: RType[A]) extends RType[ReplColl[A]] {
    val classTag: ClassTag[ReplColl[A]] = ClassTag[ReplColl[A]](classOf[ReplColl[A]])
    override def name: String = s"ReplColl[${tItem.name}]"
    override def isConstantSize: Boolean = false
  }
}

package object collection {
  implicit def collRType[A](implicit tA: RType[A]): RType[Coll[A]] = CollType[A](tA)
  implicit def extendCollType[A](ct: RType[Coll[A]]): CollType[A] = ct.asInstanceOf[CollType[A]]

  implicit def replCollRType[A](implicit tA: RType[A]): RType[ReplColl[A]] = ReplCollType[A](tA)

  implicit val collBuilderRType: RType[CollBuilder] = RType.fromClassTag(classTag[CollBuilder])
}
