package sigmastate.utils

import scalan.util.FileUtil
import sigmastate.serialization.ValueSerializer.{serializerInfo, getSerializer}

/** Generate contents of ErgoTree serializer format specification.
  */
object GenSerializers extends SpecGen {

  def printSerializerFormats() = {
    serializerInfo.map { case (_, s) =>
      s.toString
    }.mkString("\n")
  }

  def generateSerSpec() = {
    val fileName = "ergotree_serialization.tex"
    val formatsTex = printSerializerFormats()
    val file = FileUtil.file(s"docs/spec/generated/$fileName")
    FileUtil.write(file, formatsTex)

    println(s"\\input{generated/$fileName}")
  }

}
