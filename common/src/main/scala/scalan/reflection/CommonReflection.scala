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

  def registerClassOnly(cls: Class[_]) = registerClassEntry(cls)

  registerClassOnly(classOf[Array[Byte]])
  registerClassOnly(classOf[Boolean])
  registerClassOnly(classOf[Byte])

  registerClassOnly(classOf[Int])

  registerClassOnly(classOf[java.lang.Integer])

  registerClassOnly(classOf[java.lang.Object])

  registerClassOnly(classOf[java.lang.String])

  registerClassOnly(classOf[Long])

  registerClassOnly(classOf[Short])

  registerClassOnly(classOf[scala.Product2[_,_]])

  registerClassOnly(classOf[scala.collection.IndexedSeq[_]])

  registerClassOnly(classOf[scala.collection.Seq[_]])

  registerClassOnly(classOf[scala.collection.immutable.List[_]])

  registerClassOnly(classOf[scala.collection.immutable.Map[_,_]])

  class SRClassBuilder[T](clazz: Class[T]) {

  }

  { val clazz = classOf[scala.Option[_]]
    registerClassEntry(clazz,
      methods = Map(
        { val name = "filter"
          val paramTypes: Seq[Class[_]] = Array(classOf[scala.Function1[_,_]])
          (name, paramTypes) ->
            new SRMethod(clazz, name, paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: Option[a] =>
                  obj.filter(args(0).asInstanceOf[Function1[a,Boolean]])
              }
            }
        },
        { val name = "map"
          val paramTypes: Seq[Class[_]] = Array(classOf[scala.Function1[_,_]])
          (name, paramTypes) ->
            new SRMethod(clazz, name, paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: Option[a] =>
                  obj.map(args(0).asInstanceOf[Function1[a,_]])
              }
            }
        }
      )
    )
  }

  { val clazz = classOf[scala.Some[_]]
    registerClassEntry(clazz,
      constructors = Array(
        new SRConstructor[Any](Array(classOf[java.lang.Object])) {
          override def newInstance(args: AnyRef*): Any =
            new scala.Some(args(0).asInstanceOf[java.lang.Object])
        }
      )
    )
  }
  {
    val clazz = classOf[scala.collection.immutable.$colon$colon[_]]
    registerClassEntry(clazz,
      constructors = Array(
        new SRConstructor[Any](Array(classOf[java.lang.Object], classOf[scala.collection.immutable.List[_]])) {
          override def newInstance(args: AnyRef*): Any =
            new scala.collection.immutable.$colon$colon(args(0), args(1).asInstanceOf[scala.collection.immutable.List[_]])
        }
      )
    )
  }
}
