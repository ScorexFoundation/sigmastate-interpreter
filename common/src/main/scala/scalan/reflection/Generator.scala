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
  
  def isEmpty(rcls: JRClass[_]): Boolean =
    (rcls.constructors == null || rcls.constructors.isEmpty) && rcls.fields.isEmpty && rcls.methods.isEmpty

  def genClass(cls: Class[_], rcls: JRClass[_], b: mutable.StringBuilder) = {
    if (isEmpty(rcls)) {
      b.append(genEmptyClass(rcls))
    }
    else {
      b.append(s"$cls {\n")
      val cs = Option(rcls.constructors).toArray.flatten
      for ( c <- cs.collect { case c: JRConstructor[_] if c.wasUsed => c } ) {
        val paramTypes = c.value.getParameterTypes
        val args = if (paramTypes.isEmpty) "()"
        else {
          paramTypes.map(c => c.getName).mkString("(", ",", ")")
        }
        b.append(s"  constructor${c.index}$args\n")
      }
      for ( (n, f) <- rcls.fields ) {
        b.append(s"  val $n: ${f.getType.getName}\n")
      }
      for ( ((n, args), m) <- rcls.methods ) {
        b.append(s"  def $n -> obj.asInstanceOf[${cls.getName}].$n(${args.zipWithIndex.rep { case (c, i) => s"args($i).asInstanceOf[${c.getName}]" }})\n")
      }
      b.append(s"}\n\n")
    }
  }

  def generateReport(): String = {
    val b = new mutable.StringBuilder(100)
    for ( (cls, rcls) <- RClass.classes.toSeq.filter(e => isEmpty(e._2)).sortBy(_._1.getName)) {
      genClass(cls, rcls, b)
    }
    for ( (cls, rcls) <- RClass.classes.toSeq.filter(e => !isEmpty(e._2)).sortBy(_._1.getName)) {
      genClass(cls, rcls, b)
    }
    b.result()
  }

}
