package sigmastate.utils

import scalan.util.FileUtil
import sigmastate.serialization.ValueSerializer._
import sigma.util.Extensions._

/** Generate contents of ErgoTree serializer format specification.
  */
object GenSerializers extends SpecGen {

  def printDataScope(dataScope: DataScope, level: Int, sb: StringBuilder) = {
    val prefix = "~~" * level
    val name = dataScope.name
    val fmt = dataScope.data.format
    val size = fmt.size
    val desc = dataScope.data.info.description
    val row =
      s"""    $prefix\\lst{$name} & \\lst{$fmt} & \\text{$size} & \\text{$desc} \\\\
         |    \\hline
      """.stripMargin
    sb.append(row)
  }

  def printForScope(scope: ForScope, level: Int, sb: StringBuilder) = {
    val prefix = "~~" * level
    val header =
      s"""    \\multicolumn{4}{l}{${prefix}\\lst{for i=1 to ${scope.limitVar}}} \\\\
        |    \\hline
         """.stripMargin
    sb.append(header)

    for ((_, s) <- scope.children) {
      printScope(s, level + 1, sb)
    }

    val footer = s"    \\multicolumn{4}{l}{${prefix}\\lst{end for}} \\\\"
    sb.append(footer)
  }

  def printOptionScope(scope: OptionScope, level: Int, sb: StringBuilder) = {
    val prefix = "~~" * level
    val header =
      s"""    \\multicolumn{4}{l}{${prefix}\\lst{optional ${scope.name}}} \\\\
         |    \\hline
         |    ${prefix}~~\\lst{tag} & \\lst{Byte} & 1 & \\text{0 - no value; 1 - has value} \\\\
         |    \\hline
         |    \\multicolumn{4}{l}{~~\\lst{when tag == 1}} \\\\
         |    \\hline
         """.stripMargin
    sb.append(header)

    for ((_, s) <- scope.children) {
      printScope(s, level + 2, sb)
    }

    val footer = s"    \\multicolumn{4}{l}{${prefix}\\lst{end optional}} \\\\"
    sb.append(footer)
  }

  def printScope(scope: Scope, level: Int, sb: StringBuilder): Unit = {
    scope match {
      case scope: DataScope =>
        printDataScope(scope, level, sb)
      case scope: ForScope =>
        printForScope(scope, level, sb)
      case scope: OptionScope =>
        printOptionScope(scope, level, sb)
      case _ =>
        sb.append(s"% skipped $scope\n")
    }
  }

  def printSerScopeSlots(serScope: SerScope) = {
    val rows = StringBuilder.newBuilder
    var betweenLines = false
    serScope.children.map { case (name, scope) =>
       printScope(scope, 0, rows)
    }
    rows.result()
  }

  def printSerializerSections() = {
    serializerInfo.filter(_._2.children.nonEmpty).map { case (_, s) =>
      val ser = getSerializer(s.opCode)
      val opCode = ser.opCode.toUByte
      val opName = ser.opDesc.typeName
      val rows = printSerScopeSlots(s)
      s"""
        |\\subsubsection{\\lst{$opName} operation (OpCode $opCode)}
        |
        |\\noindent
        |\\(\\begin{array}{| l | l | l | l |}
        |    \\hline
        |    \\bf{Slot} & \\bf{Format} & \\bf{\\#bytes} & \\bf{Description} \\\\
        |    \\hline
        |    $rows
        |\\end{array}\\)
       """.stripMargin
    }.mkString("\n")
  }

  def generateSerSpec() = {
    val fileName = "ergotree_serialization1.tex"
    val formatsTex = printSerializerSections()
    val file = FileUtil.file(s"docs/spec/generated/$fileName")
    FileUtil.write(file, formatsTex)

    println(s"\\input{generated/$fileName}")
  }

}
