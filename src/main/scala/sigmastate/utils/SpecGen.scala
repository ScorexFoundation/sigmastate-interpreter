package sigmastate.utils

import sigmastate._
import sigmastate.eval.Evaluation._
import sigmastate.eval.{Zero, Sized, Evaluation}
import SType._
import scalan.util.FileUtil
import scalan.meta.PrintExtensions._

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

  def methodSubsection(typeName: String, m: SMethod) = {
    val argTypes = m.stype.tDom.tail
    val resTpe = m.stype.tRange.toTermString
    val ts = argTypes.map(_.toTermString)
    val argInfos = m.docInfo.fold(
      Range(0, ts.length).map(i => MethodArgInfo("arg" + i, "")))(info => info.args.toIndexedSeq)
    val params = ts.opt { ts =>
      val args = argInfos.zip(ts)
      s"""
        |  \\hline
        |  \\bf{Parameters} &
        |      \\(\\begin{array}{l l l}
        |         ${args.rep({ case (info, t) =>
                    s"\\lst{${info.name}} & \\lst{: $t} & \\text{// ${info.description}} \\\\"
                   }, "\n")}
        |      \\end{array}\\) \\\\
       """.stripMargin
    }

   s"""
     |\\subsubsection{\\lst{$typeName.${m.name}} method (Code ${m.objType.typeId}.${m.methodId})}
     |\\noindent
     |\\begin{tabularx}{\\textwidth}{| l | X |}
     |   \\hline
     |   \\bf{Description} & ${m.docInfo.opt(i => i.description + (!i.isOpcode).opt(" (FRONTEND ONLY)"))} \\\\
     |  $params
     |  \\hline
     |  \\bf{Result} & \\lst{${resTpe}} \\\\
     |  \\hline
     |\\end{tabularx}
     |""".stripMargin
  }

  def printMethods(typeName: String, tc: STypeCompanion, t: SType) = {
    val methodSubsections = for { m <- tc.methods.sortBy(_.methodId) } yield {

      methodSubsection(typeName, m)
    }
    val res = methodSubsections.mkString("\n\n")
    res
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
    val typeName = tc match {
      case t: SType =>
        val rtype = stypeToRType(t)
        rtype.name
      case _ => tc.getClass.getSimpleName.replace("$", "")
    }
    val methodsRows = printMethods(typeName, tc, t)
    val fMethods = FileUtil.file(s"docs/spec/generated/${typeName}_methods.tex")
    FileUtil.write(fMethods, methodsRows)

    println(s"\\input{generated/${typeName}_methods.tex}")
  }
}
