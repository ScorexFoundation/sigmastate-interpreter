package sigmastate.utils

import sigmastate._
import sigmastate.eval.Evaluation._
import sigmastate.eval.{Zero, Sized, Evaluation}
import sigma.util.Extensions.ByteOps
import SType._
import scalan.util.{CollectionUtil, FileUtil}
import scalan.meta.PrintExtensions._
import sigmastate.Values.ValueCompanion
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{ValueSerializer, OpCodes}

import scala.collection.immutable

object SpecGenUtils {
  val types = SType.allPredefTypes.diff(Seq(SString))
  val companions: Seq[STypeCompanion] = types.collect { case tc: STypeCompanion => tc }
  val typesWithMethods = companions ++ Seq(SCollection, SOption)
}

trait SpecGen {
  import SpecGenUtils._
  val tT = STypeIdent("T")

  def collectSerializers(): Seq[ValueSerializer[_ <: Values.Value[SType]]] = {
    ((OpCodes.LastConstantCode + 1) to 255).collect {
      case i if ValueSerializer.serializers(i.toByte) != null =>
        val ser = ValueSerializer.serializers(i.toByte)
        assert(i == ser.opDesc.opCode.toUByte)
        ser
    }
  }

  def collectFreeCodes(): Seq[Int] = {
    ((OpCodes.LastConstantCode + 1) to 255).collect {
      case i if ValueSerializer.serializers(i.toByte) == null => i
    }
  }

  def collectMethods(): Seq[SMethod] = {
    for {
      tc <- typesWithMethods.sortBy(_.typeId)
      m <- tc.methods.sortBy(_.methodId)
    } yield m
  }

  def collectOperations(): Seq[(OpCode, ValueCompanion)] = {
    val sers = collectSerializers()
    sers.map(s => (s.opCode, s.opDesc))
  }

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
    val serializedAs = m.irInfo.opDesc.opt(d =>
      s"""
        |  \\bf{Serialized as} & \\lst{${d.typeName}(opCode=${d.opCode.toUByte})} \\\\
        |  \\hline
       """.stripMargin
    )
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
      |  $serializedAs
      |\\end{tabularx}
      |""".stripMargin
  }

  def printMethods(tc: STypeCompanion) = {
    val methodSubsections = for { m <- tc.methods.sortBy(_.methodId) } yield {
      methodSubsection(tc.typeName, m)
    }
    val res = methodSubsections.mkString("\n\n")
    res
  }
}

object GenPredeftypesApp extends SpecGen {
  import SpecGenUtils._

  def main(args: Array[String]) = {
    val table = printTypes(companions)
    val fPrimOps = FileUtil.file("docs/spec/generated/predeftypes.tex")
    FileUtil.write(fPrimOps, table)

    for (tc <- typesWithMethods) {
      val typeName = tc.typeName
      val methodsRows = printMethods(tc)
      val fMethods = FileUtil.file(s"docs/spec/generated/${typeName}_methods.tex")
      FileUtil.write(fMethods, methodsRows)

      println(s"\\input{generated/${typeName}_methods.tex}")
    }
  }
}

object PrintSerializersInfoApp extends SpecGen {
  def main(args: Array[String]) = {
    val sers = collectSerializers()

    val serTypes =  sers.groupBy(s => s.getClass.getSimpleName)
    for ((name, group) <- serTypes) {
      println(name + ":")
      for (ser <- group) {
        val line = s"\t${ser.opCode.toUByte} -> serializer: ${ser.getClass.getSimpleName}; opDesc: ${ser.opDesc}"
        println(line)
      }
    }
    println(s"Total Serializable Ops: ${sers.length}")
    println(s"Total Serializer Types: ${serTypes.keys.size}")

    val ops = ValueCompanion.allOperations
    for ((k,v) <- ops.toArray.sortBy(_._1.toUByte)) {
      println(s"${k.toUByte} -> $v")
    }
    println(s"Total ops: ${ops.size}")
  }
}

object GenPrimOpsApp extends SpecGen {
  def main(args: Array[String]) = {
    val methods = collectMethods()
    val ops = collectOperations()
    val primOps = CollectionUtil.outerJoinSeqs(ops, methods)(
      o => Some(o._1), m => m.irInfo.opDesc.map(_.opCode)
    )(
      (k, o) => Some(o), // left without right
      (k,i) => None,     // right without left
      (k,i,o) => None    // left and right
    ).map(_._2).collect { case Some(op) => op }

    for (p <- primOps)
      println(s"$p")
  }
}
