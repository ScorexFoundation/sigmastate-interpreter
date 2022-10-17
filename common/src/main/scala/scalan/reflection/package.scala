package scalan

package object reflection {

  def mkMethod(clazz: Class[_], name: String, paramTypes: Seq[Class[_]])
      (handler: (Any, Array[AnyRef]) => Any): ((String, Seq[Class[_]]), RMethod) = {
    (name, paramTypes) ->
        new SRMethod(clazz, name, paramTypes) {
          override def invoke(
              obj: Any,
              args: AnyRef*): Any = handler(obj, args.toArray)
        }
  }

  def mkConstructor(parameterTypes: Array[Class[_]])(handler: Array[AnyRef] => Any) = {
    new SRConstructor[Any](parameterTypes) {
      override def newInstance(args: AnyRef*): Any = handler(args.toArray)
    }
  }

}
