package scalan.reflection

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable

object ReflectionData {
  def registerClassEntry[T](clazz: Class[T],
                            constructors: Seq[SRConstructor[_]],
                            fields: Map[String, SRField],
                            methods: Map[(String, Seq[Class[_]]), RMethod]): Unit = classes.synchronized {
    classes.put(clazz, new SRClass(clazz, constructors, fields, methods))
  }

  def registerEmptyClass(cls: Class[_]) =
    registerClassEntry(cls, constructors = ArraySeq.empty, fields = Map.empty, methods = Map.empty)

  val classes = mutable.HashMap.empty[Class[_], SRClass[_]]

  registerEmptyClass(classOf[Array[Byte]])
  registerEmptyClass(classOf[Boolean])
  registerEmptyClass(classOf[Byte])

  registerEmptyClass(classOf[Int])

  registerEmptyClass(classOf[java.lang.Integer])

  registerEmptyClass(classOf[java.lang.Object])

  registerEmptyClass(classOf[Long])

  registerEmptyClass(classOf[scala.Product2[_,_]])

  registerEmptyClass(classOf[scala.collection.IndexedSeq[_]])

  registerEmptyClass(classOf[scala.collection.Seq[_]])

  registerEmptyClass(classOf[scala.collection.immutable.List[_]])
}
