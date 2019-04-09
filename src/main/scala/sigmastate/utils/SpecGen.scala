package sigmastate.utils

import sigmastate._
import sigmastate.eval.Evaluation._
import sigmastate.eval.{Zero, Sized}
import SType._
import scalan.util.FileUtil

object SpecGen extends App {
  val tT = STypeIdent("T")
  val tT1 = STypeIdent("T_1")
  val tTn = STypeIdent("T_n")

  def printTypes(companions: Seq[STypeCompanion]) = {
    val lines = for { tc <- companions.sortBy(_.typeId) } yield {
      val t = tc match {
        case t: SType => t
        case SCollection => SCollection(tT)
        case SOption => SOption(tT)
      }
      val rtype = stypeToRType(t)
      val name = t match { case SGlobal => "Global" case _ => rtype.name }
      val isConst = t.isConstantSize
      val isPrim = t.isInstanceOf[SPrimType]
      val isEmbed = t.isInstanceOf[SEmbeddable]
      val isNum = t.isInstanceOf[SNumericType]
      val valRange = t match {
        case SBoolean => s"$$\\Set{\\lst{true}, \\lst{false}}$$"
        case n: SNumericType =>
          val s = Sized.typeToSized(rtype)
          val z = Zero.typeToZero(rtype).zero
          val bits = s.size(z).dataSize * 8 - 1
          s"$$\\Set{-2^{$bits} \\dots 2^{$bits}-1}$$~\\ref{sec:type:${name}}"
        case SGroupElement => s"$$\\Set{p \\in \\lst{SecP256K1Point}}$$"
        case _ => s"Sec.~\\ref{sec:type:${name}}"
      }
      val line =
        s"""\\lst{$name}	&	$$${tc.typeId}$$	&	\\lst{$isConst}	& \\lst{$isPrim}	&	\\lst{$isEmbed} &	\\lst{$isNum}	& $valRange \\\\"""
      line
    }
    val table = lines.mkString("\n\\hline\n")
    table
  }

  def printMethods(tc: STypeCompanion, t: SType) = {
    val desc = ""
    val lines = for { m <- tc.methods.sortBy(_.methodId) } yield {
      s"${m.objType.typeId}.${m.methodId} & \\lst{def ${m.name}()} & $desc \\\\"
    }
    val table = lines.mkString("\n\\hline\n")
    s"""
      |\\noindent
      |\\begin{tabularx}{\\textwidth}{| c | c | X |}
      |  \\hline
      |  \\bf{Code} & \\bf{Method Signature} & \\bf{Description} \\\\
      |  \\hline
      |  $table
      |  \\hline
      |\\end{tabularx}
     """.stripMargin
  }

  val types = SType.allPredefTypes.diff(Seq(SString))
  val companions: Seq[STypeCompanion] = types.collect { case tc: STypeCompanion => tc }
  val table = printTypes(companions)
  val fPrimOps = FileUtil.file("docs/spec/generated/primops.tex")
  FileUtil.write(fPrimOps, table)

  val typesWithMethods =
        companions.zip(companions.asInstanceOf[Seq[SType]]) ++
        Seq((SCollection, SCollection.ThisType), (SOption, SOption.ThisType))
  for ((tc, t) <- typesWithMethods) {
    val methodsRows = printMethods(tc, t)
    val name = tc match {
      case t: SType =>
        val rtype = stypeToRType(t)
        rtype.name
      case _ => tc.getClass.getSimpleName.replace("$", "")
    }
    val fMethods = FileUtil.file(s"docs/spec/generated/${name}_methods.tex")
    FileUtil.write(fMethods, methodsRows)

    println(s"\\input{generated/${name}_methods.tex}")
  }
}
