package scalan.reflection

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable

object CommonReflection {
  val classes = mutable.HashMap.empty[Class[_], SRClass[_]]

  def registerClassEntry[T](clazz: Class[T],
                            constructors: Seq[SRConstructor[_]] = ArraySeq.empty,
                            fields: Map[String, SRField] = Map.empty,
                            methods: Map[(String, Seq[Class[_]]), RMethod] = Map.empty): Unit = classes.synchronized {
    classes.put(clazz, new SRClass(clazz, constructors, fields, methods))
  }

  registerClassEntry(classOf[Boolean])

  registerClassEntry(classOf[Byte])

  registerClassEntry(classOf[Short])

  registerClassEntry(classOf[Int])

  registerClassEntry(classOf[Long])

  registerClassEntry(classOf[Product2[_, _]])

  //  registerClassEntry(classOf[Array[Byte]])
//
//  registerClassEntry(classOf[java.lang.Integer])
//
//  registerClassEntry(classOf[java.lang.Object])
//
//  registerClassEntry(classOf[java.lang.String])
//
//  registerClassEntry(classOf[Long])
//

//  registerClassEntry(classOf[scala.Product2[_,_]])
//
//  registerClassEntry(classOf[scala.collection.IndexedSeq[_]])
//
//  registerClassEntry(classOf[scala.collection.Seq[_]])
//
//  registerClassEntry(classOf[scala.collection.immutable.List[_]])
//
//  registerClassEntry(classOf[scala.collection.immutable.Map[_,_]])
//
//  class SRClassBuilder[T](clazz: Class[T]) {
//
//  }

  { val clazz = classOf[scala.Option[_]]
    registerClassEntry(clazz,
      methods = Map(
        mkMethod(clazz, "filter", Array(classOf[scala.Function1[_,_]])) { (obj, args) =>
          obj.asInstanceOf[Option[Any]].filter(args(0).asInstanceOf[Any => Boolean])
        },
        mkMethod(clazz, "map", Array(classOf[scala.Function1[_,_]])) { (obj, args) =>
          obj.asInstanceOf[Option[Any]].map(args(0).asInstanceOf[Any => Any])
        }
      )
    )
  }

  { val clazz = classOf[scala.Some[_]]
    registerClassEntry(clazz,
      constructors = Array(
        mkConstructor(Array(classOf[java.lang.Object])) { args =>
          new scala.Some(args(0).asInstanceOf[java.lang.Object])
        }
      )
    )
  }

//  {
//    val clazz = classOf[scala.collection.immutable.$colon$colon[_]]
//    registerClassEntry(clazz,
//      constructors = Array(
//        new SRConstructor[Any](Array(classOf[java.lang.Object], classOf[scala.collection.immutable.List[_]])) {
//          override def newInstance(args: AnyRef*): Any =
//            new scala.collection.immutable.$colon$colon(args(0), args(1).asInstanceOf[scala.collection.immutable.List[_]])
//        }
//      )
//    )
//  }
}
