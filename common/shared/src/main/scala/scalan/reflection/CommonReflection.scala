package scalan.reflection

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.immutable

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

  registerClassEntry(classOf[immutable.$colon$colon[_]],
    constructors = Array(
      mkConstructor(Array(classOf[java.lang.Object], classOf[immutable.List[_]])) { args =>
        new immutable.$colon$colon(args(0).asInstanceOf[java.lang.Object], args(1).asInstanceOf[immutable.List[_]])
      }
    )
  )

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

}
