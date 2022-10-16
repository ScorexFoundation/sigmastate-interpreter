package special

import scala.language.implicitConversions
import scalan.RType
import scalan.reflection.CommonReflection.registerClassEntry
import scalan.reflection.SRMethod

import scala.reflect.{classTag, ClassTag}

package collection {
  case class CollType[A](tItem: RType[A]) extends RType[Coll[A]] {
    val classTag: ClassTag[Coll[A]] = ClassTag[Coll[A]](classOf[Coll[A]])
    override def name: String = s"Coll[${tItem.name}]"
  }
}

package object collection {
  implicit def collRType[A](implicit tA: RType[A]): RType[Coll[A]] = CollType[A](tA)
  implicit def extendCollType[A](ct: RType[Coll[A]]): CollType[A] = ct.asInstanceOf[CollType[A]]
  implicit val collBuilderRType: RType[CollBuilder] = RType.fromClassTag(classTag[CollBuilder])
}
