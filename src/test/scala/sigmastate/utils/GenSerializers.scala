package sigmastate.utils

import scalan.util.FileUtil
import sigmastate.serialization.ValueSerializer._
import sigma.util.Extensions._

/** Generate contents of ErgoTree serializer format specification.
  */
object GenSerializers extends SpecGen {

  def printDataScope(dataScope: DataScope, level: Int, sb: StringBuilder) = {
    val prefix = "~" * level
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

  def printSerScopeSlots(serScope: SerScope) = {
    val rows = StringBuilder.newBuilder
    var betweenLines = false
    serScope.children.map { case (name, scope) =>
      scope match {
        case scope: DataScope =>
          if (betweenLines) {
            rows.append("    \\hline\n")
            betweenLines = false
          }
          printDataScope(scope, 1, rows)
        case _ =>
          rows.append(s"% skipped $scope\n")
      }
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
