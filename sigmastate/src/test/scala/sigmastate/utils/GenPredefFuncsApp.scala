package sigmastate.utils

import scalan.util.FileUtil
import scalan.util.PrintExtensions._

object GenPredefFuncsApp extends SpecGen {

  def main(args: Array[String]) = {
    val opsTable = filterOutDisabled(collectOpsTable())
    val opInfos = opsTable.collect { case (d, m, optF @ Some(f)) =>
      val info = getOpInfo(d, m, optF)
      (d, f, info)
    }.sortBy(i => toDisplayCode(i._1.opCode))

    val funcRows = StringBuilder.newBuilder
    val sections = StringBuilder.newBuilder

    for (row @ (d, f, info) <- opInfos) {
      val opCode = toDisplayCode(d.opCode)
      val mnemonic = d.typeName
      val opName = toTexName(f.name)
      val argsTpe = f.declaration.tpe.tDom.rep(_.toTermString)
      val resTpe = f.declaration.tpe.tRange.toTermString
      val serRef = s"\\hyperref[sec:serialization:operation:$mnemonic]{\\lst{$mnemonic}}"
      val desc = if (info.description.length > 150) {
        s"See~\\ref{sec:appendix:primops:${f.docInfo.opTypeName}}"
      } else
        s"${info.description} See~\\ref{sec:appendix:primops:${f.docInfo.opTypeName}}"

      funcRows.append(
        s""" $opCode & $serRef & $desc \\\\
          | \\hline
         """.stripMargin)

      val subsection = funcSubsection(f)
      sections.append(subsection)
    }
    println(s"Total ops: ${opInfos.length}")

    saveFile("docs/spec/generated/predeffunc_rows.tex", funcRows.result())
    saveFile("docs/spec/generated/predeffunc_sections.tex", sections.result())
  }
}
