package special

import scala.language.implicitConversions
import scalan.RType
import scalan.reflection.ReflectionData.registerClassEntry
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

  { val clazz = classOf[special.collection.Coll[_]]
    registerClassEntry(clazz,
      methods = Map(
      { val paramTypes: Seq[Class[_]] = Array(classOf[Int], classOf[java.lang.Object])
        ("updated", paramTypes) ->
          new SRMethod(clazz, "updated", paramTypes) {
            override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
              case obj: Coll[a] =>
                obj.updated(args(0).asInstanceOf[Int], args(1).asInstanceOf[a])
            }
          }
      },
      { val paramTypes: Seq[Class[_]] = Array(classOf[java.lang.Object], classOf[Int])
        ("indexOf", paramTypes) ->
          new SRMethod(clazz, "indexOf", paramTypes) {
            override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
              case obj: Coll[a] =>
                obj.indexOf(args(0).asInstanceOf[a], args(1).asInstanceOf[Int]).asInstanceOf[AnyRef]
            }
          }
      },
      { val paramTypes: Seq[Class[_]] = Array(classOf[Function1[_,_]], classOf[RType[_]])
        ("flatMap", paramTypes) ->
          new SRMethod(clazz, "flatMap", paramTypes) {
            override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
              case obj: Coll[a] =>
                obj.flatMap(args(0).asInstanceOf[a => Coll[Any]])(args(1).asInstanceOf[RType[Any]])
            }
          }
      }
      )
    )
  }

}
