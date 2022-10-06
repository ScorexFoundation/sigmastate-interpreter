package scalan.reflection

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable

object ReflectionData {
  def CLS[T](clazz: Class[T],
             constructors: Seq[SRConstructor[_]],
             fields: Map[String, SRField],
             methods: Map[(String, Seq[Class[_]]), RMethod]): (Class[T], SRClass[T]) = {
    clazz -> new SRClass(clazz, constructors, fields, methods)
  }

  val classes:  mutable.HashMap[Class[_], SRClass[_]] = mutable.HashMap(
    CLS(classOf[java.lang.String],
      constructors = ArraySeq.empty,
      fields = Map.empty,
      methods = Map.empty
    )
  )

}
