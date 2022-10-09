package scalan.reflection

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable

object ReflectionData {
  val classes = mutable.HashMap.empty[Class[_], SRClass[_]]

  def registerClassEntry[T](clazz: Class[T],
                            constructors: Seq[SRConstructor[_]] = ArraySeq.empty,
                            fields: Map[String, SRField] = Map.empty,
                            methods: Map[(String, Seq[Class[_]]), RMethod] = Map.empty): Unit = classes.synchronized {
    classes.put(clazz, new SRClass(clazz, constructors, fields, methods))
  }

  def registerEmptyClass(cls: Class[_]) = registerClassEntry(cls)

  registerEmptyClass(classOf[Array[Byte]])
  registerEmptyClass(classOf[Boolean])
  registerEmptyClass(classOf[Byte])

  registerEmptyClass(classOf[Int])

  registerEmptyClass(classOf[java.lang.Integer])

  registerEmptyClass(classOf[java.lang.Object])

  registerEmptyClass(classOf[java.lang.String])

  registerEmptyClass(classOf[Long])

  registerEmptyClass(classOf[Short])

  registerEmptyClass(classOf[scala.Product2[_,_]])

  registerEmptyClass(classOf[scala.collection.IndexedSeq[_]])

  registerEmptyClass(classOf[scala.collection.Seq[_]])

  registerEmptyClass(classOf[scala.collection.immutable.List[_]])

  registerEmptyClass(classOf[scala.collection.immutable.Map[_,_]])

  class SRClassBuilder[T](clazz: Class[T]) {

  }

  { val clazz = classOf[scala.Option[_]]
    registerClassEntry(clazz,
      methods = Map(
        { val name = "filter"
          val paramTypes: Seq[Class[_]] = Array(classOf[scala.Function1[_,_]])
          (name, paramTypes) ->
            new SRMethod(RClass(clazz), name, paramTypes) {
              override def invoke(obj: Any, args: AnyRef*): AnyRef = obj match {
                case obj: Option[a] =>
                  obj.filter(args(0).asInstanceOf[Function1[a,Boolean]])
              }
            }
        },
        { val name = "map"
          val paramTypes: Seq[Class[_]] = Array(classOf[scala.Function1[_,_]])
          (name, paramTypes) ->
            new SRMethod(RClass(clazz), name, paramTypes) {
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

}
