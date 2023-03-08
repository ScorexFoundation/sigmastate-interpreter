package scalan

package object reflection {
  /** Creates a new SRMethod instance with the given parameters and handler function.
    *
    * @param clazz      the [[java.lang.Class]] that declares the method
    * @param name       the name of the method
    * @param paramTypes the types of the method's parameters
    * @param handler    a function that handles invoking the method, when RMethod.invoke
    *                   is called then this `handler` is called
    * @return a tuple containing the method's name and parameter types as its first element,
    *         and an SRMethod instance as its second element
    */
  def mkMethod(clazz: Class[_], name: String, paramTypes: Seq[Class[_]])
      (handler: (Any, Array[AnyRef]) => Any): ((String, Seq[Class[_]]), RMethod) = {
    (name, paramTypes) ->
        new SRMethod(clazz, name, paramTypes) {
          override def invoke(
              obj: Any,
              args: AnyRef*): Any = handler(obj, args.toArray)
        }
  }

  /** Creates a new [[SRConstructor]] instance with the given parameter types and handler function.
    *
    * @param parameterTypes the types of the constructor's parameters
    * @param handler        a function that handles creating a new instance using this constructor
    * @return an SRConstructor instance
    */
  def mkConstructor(parameterTypes: Array[Class[_]])(handler: Array[AnyRef] => Any) = {
    new SRConstructor[Any](parameterTypes) {
      override def newInstance(args: AnyRef*): Any = handler(args.toArray)
    }
  }

}
