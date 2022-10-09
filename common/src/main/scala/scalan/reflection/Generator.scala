package scalan.reflection

import scala.collection.mutable
import scalan.util.PrintExtensions.IterableExtensions

object Generator {

  def className(c: Class[_]): String = {
    if (c.isArray)
      s"Array[${className(c.getComponentType)}]"
    else if (c == classOf[Boolean]) "Boolean"
    else if (c == classOf[Byte]) "Byte"
    else if (c == classOf[Short]) "Short"
    else if (c == classOf[Int]) "Int"
    else if (c == classOf[Long]) "Long"
    else if (!c.getTypeParameters.isEmpty)
      s"${c.getName}[${c.getTypeParameters.map(_ => "_").mkString(",")}]"
    else
      c.getName
  }

  def genEmptyClass(c: JRClass[_]): String = {
    val name = className(c.value)
    s"registerEmptyClass(classOf[$name])\n"
  }

  def genConstructor(c: JRClass[_], ctor: JRConstructor[_]): String = {
    val params = ctor.getParameterTypes()
      .map(p => s"classOf[${className(p)}]")
      .mkString(", ")
    val clsName = c.getName
    val args = ctor.getParameterTypes()
      .zipWithIndex
      .map { case (p, i) => s"args($i).asInstanceOf[${className(p)}]" }
      .mkString(", ")

    s"""        new SRConstructor[Any](Array($params)) {
      |          override def newInstance(args: AnyRef*): Any =
      |            new $clsName($args)
      |        }""".stripMargin
  }

  def genField(c: JRClass[_], f: JRField): String = {
    f.toString
  }

  def genClassRegistrationEntry(c: JRClass[_]): String = {
    val name = className(c.value)
    val cs = c.getUsedConstructors()
    val constructors = if (cs.isEmpty) ""
    else {
      s""",
        |    constructors = Array(
        |${cs.map(genConstructor(c, _)).mkString(",\n")}
        |    )""".stripMargin
    }
    val fields = if (c.fields.isEmpty) ""
    else {
      s""",
        |    fields = Array(
        |      ${c.fields.map { case (_, f: JRField) => genField(c, f) }.mkString(",\n")}
        |    )""".stripMargin
    }

    s"""
      |{ val clazz = classOf[$name]
      |  registerClassEntry(clazz${constructors}${fields}
      |  )
      |}
      |""".stripMargin
  }

  def isEmpty(rcls: JRClass[_]): Boolean =
    (rcls.constructors == null || rcls.constructors.isEmpty) && rcls.fields.isEmpty && rcls.methods.isEmpty

  def genClass(cls: Class[_], rcls: JRClass[_], b: mutable.StringBuilder) = {
    if (isEmpty(rcls)) {
      b.append(genEmptyClass(rcls))
    }
    else {
      b.append(genClassRegistrationEntry(rcls))
//      b.append(s"$cls {\n")
//      for ( c <- rcls.getUsedConstructors() ) {
//        val paramTypes = c.value.getParameterTypes
//        val args = if (paramTypes.isEmpty) "()"
//        else {
//          paramTypes.map(c => c.getName).mkString("(", ",", ")")
//        }
//        b.append(s"  constructor${c.index}$args\n")
//      }
//      for ( (n, f) <- rcls.fields ) {
//        b.append(s"  val $n: ${f.getType.getName}\n")
//      }
//      for ( ((n, args), m) <- rcls.methods ) {
//        b.append(s"  def $n -> obj.asInstanceOf[${cls.getName}].$n(${args.zipWithIndex.rep { case (c, i) => s"args($i).asInstanceOf[${c.getName}]" }})\n")
//      }
//      b.append(s"}\n\n")
    }
  }

  def generateReport(): String = {
    val b = new mutable.StringBuilder(100)
    for ((cls, rcls) <- collectEmptyClasses.sortBy(_._1.getName)) {
      genClass(cls, rcls, b)
    }
    for ( (cls, rcls) <- collectNonEmptyClasses.sortBy(_._1.getName) ) {
      genClass(cls, rcls, b)
    }
    b.result()
  }

  private def collectNonEmptyClasses = {
    RClass.classes.toSeq.filter(e => !isEmpty(e._2))
  }

  private def collectEmptyClasses = {
    RClass.classes.toSeq.filter(e => isEmpty(e._2))
  }
}
