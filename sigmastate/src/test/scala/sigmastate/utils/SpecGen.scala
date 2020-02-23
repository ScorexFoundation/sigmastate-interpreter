package sigmastate.utils

import sigmastate._
import sigmastate.eval.Evaluation._
import sigmastate.eval.{Zero, Sized}
import scalan.util.Extensions.ByteOps
import scalan.util.{CollectionUtil, FileUtil}
import scalan.util.PrintExtensions._
import sigmastate.Values.{FalseLeaf, Constant, TrueLeaf, BlockValue, ConstantPlaceholder, Tuple, ValDef, FunDef, ValUse, ValueCompanion, TaggedVariable, ConcreteCollection, ConcreteCollectionBooleanConstant}
import sigmastate.lang.SigmaPredef.{PredefinedFuncRegistry, PredefinedFunc}
import sigmastate.lang.StdSigmaBuilder
import sigmastate.lang.Terms.{PropertyCall, MethodCall}
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{ValueSerializer, OpCodes}
import sigmastate.utxo.{SigmaPropIsProven, SelectField}

object SpecGenUtils {
  val types = SType.allPredefTypes.diff(Seq(SString))
  val typeCompanions: Seq[STypeCompanion] = types.collect { case tc: STypeCompanion => tc }
  val typesWithMethods = typeCompanions ++ Seq(SCollection, SOption)
}

trait SpecGen {
  import SpecGenUtils._
  val tT = STypeVar("T")

  def saveFile(fileName: String, text: String) = {
    val fPrimOps = FileUtil.file(fileName)
    FileUtil.write(fPrimOps, text)
  }

  case class OpInfo(
      opDesc: ValueCompanion,
      description: String,
      args: Seq[ArgInfo], op: Either[PredefinedFunc, SMethod])

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

  def collectSerializableOperations(): Seq[(OpCode, ValueCompanion)] = {
    val sers = collectSerializers()
    sers.map(s => (s.opCode, s.opDesc))
  }

  protected val predefFuncRegistry = new PredefinedFuncRegistry(StdSigmaBuilder)
  val noFuncs: Set[ValueCompanion] = Set(Constant, MethodCall, PropertyCall)
  val predefFuncs: Seq[PredefinedFunc] = predefFuncRegistry.funcs.values
      .filterNot { f => f.docInfo.opDesc.exists(noFuncs.contains) }.toSeq
  val specialFuncs: Seq[PredefinedFunc] = predefFuncRegistry.specialFuncs.values.toSeq

  def collectOpsTable() = {
    val ops = collectSerializableOperations().filterNot { case (_, opDesc) => noFuncs.contains(opDesc) }
    val methods = collectMethods()
    val funcs = predefFuncs ++ specialFuncs

    val methodsByOpCode = methods
        .groupBy(_.docInfo.flatMap(i => i.opDesc.map(_.opCode)))
    //      .map { case p @ (k, xs) => p.ensuring({ k.isEmpty || xs.length == 1}, p) }

    val funcsByOpCode = funcs
        .groupBy(_.docInfo.opDesc.map(_.opCode))
        .ensuring(g => g.forall{ case (k, xs) => xs.length <= 1})

    val table = ops.map { case (opCode, opDesc) =>
      val methodOpt = methodsByOpCode.get(Some(opCode)).map(_.head)
      val funcOpt = funcsByOpCode.get(Some(opCode)).map(_.head)
      (opCode, opDesc, methodOpt, funcOpt)
    }
    val rowsWithInfo =
      for ((opCode, opDesc, optM, optF) <- table if optM.nonEmpty || optF.nonEmpty)
        yield (opDesc, optM, optF)
    rowsWithInfo
  }

  def getOpInfo(opDesc: ValueCompanion, optM: Option[SMethod], optF: Option[PredefinedFunc]): OpInfo = {
    (optM, optF) match {
      case (_, Some(f)) =>
        val description = f.docInfo.description
        val args = f.docInfo.args
        OpInfo(opDesc, description, args, Left(f))
      case (Some(m), _) =>
        val description = m.docInfo.map(i => i.description).opt()
        val args = m.docInfo.map(i => i.args).getOrElse(Seq())
        OpInfo(opDesc, description, args, Right(m))
      case p => sys.error(s"Unexpected $opDesc with $p")
    }
  }

  def toTexName(name: String) = name.replace("%", "\\%")

  def toDisplayCode(opCode: OpCode): Int = opCode.toUByte

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
  
  def methodSubsection(tc: STypeCompanion, m: SMethod) = {
    val typeName = tc.typeName
    val argTypes = m.stype.tDom
    val resTpe = m.stype.tRange
    val resTpeStr = resTpe.toTermString
    val types = argTypes.map(_.toTermString)
    val argInfos = m.docInfo.fold(
      Range(0, types.length).map(i => ArgInfo("arg" + i, "")))(info => info.args.toIndexedSeq)

    val castOpt = tc match {
      case n: sigmastate.SNumericType => SNumericType.getNumericCast(n, m.name, resTpe)
      case _ => None
    }
    val serializedAs = m.docInfo.flatMap(_.opDesc).opt { d =>
      val opName = castOpt.getOrElse(d).typeName
      val opCode = d.opCode.toUByte
      s"""
        |  \\bf{Serialized as} & \\hyperref[sec:serialization:operation:$opName]{\\lst{$opName}} \\\\
        |  \\hline
       """.stripMargin
    }
    val tpeParams = m.stype.tpeParams.filterNot(tc.typeParams.contains).opt(ps => s"$$[$$${ps.map(p => s"\\lst{$p}").rep()}$$]$$")
    val sigArgs = argInfos.zip(types).drop(1).opt(args => s"(${args.map { case (info, ty) => s"""\\lst{${info.name}}$$:$$~\\lst{$ty}""" }.rep()})")
    val sig = s"""\\lst{def ${m.name}}$tpeParams$sigArgs: \\lst{$resTpeStr}"""
    subsectionTempl(
      opName = s"$typeName.${m.name}",
      opCode = s"${m.objType.typeId}.${m.methodId}",
      label  = s"sec:type:$typeName:${m.name}",
      desc = m.docInfo.opt(i => i.description + i.isFrontendOnly.opt(" (FRONTEND ONLY)")),
      signature = if (sig.length > 100) "\\footnotesize " + sig else sig,
      types = types,
      argInfos = argInfos,
      resTpe = resTpeStr,
      serializedAs = serializedAs
      )
  }

  def funcSubsection(f: PredefinedFunc) = {
    val argTypes = f.declaration.tpe.tDom
    val resTpe = f.declaration.tpe.tRange.toTermString
    val types = argTypes.map(_.toTermString)
    val argInfos = f.docInfo.args
    val opDesc = f.docInfo.opDesc
    val serializedAs = {
      val nodeName = f.docInfo.opTypeName
      s"""
        |  \\bf{Serialized as} & \\hyperref[sec:serialization:operation:$nodeName]{\\lst{$nodeName}} \\\\
        |  \\hline
       """.stripMargin
    }
    subsectionTempl(
      opName = toTexName(f.name),
      opCode = opDesc.map(_.opCode.toUByte.toString).getOrElse("NA"),
      label  = s"sec:appendix:primops:${f.docInfo.opTypeName}",
      desc = f.docInfo.description + f.docInfo.isFrontendOnly.opt(" (FRONTEND ONLY)"),
      signature = "",
      types = types,
      argInfos = argInfos,
      resTpe = resTpe,
      serializedAs = serializedAs
    )
  }

  def subsectionTempl(opName: String, opCode: String, label: String, desc: String, signature: String, types: Seq[String], argInfos: Seq[ArgInfo], resTpe: String, serializedAs: String) = {

    val params = argInfos.zip(types).filter { case (a, _) => a.name != "this" }.opt { args =>
      s"""
        |  \\hline
        |  \\bf{Parameters} &
        |      \\(\\begin{array}{l l}
        |         ${args.rep({ case (info, t) =>
                    s"\\lst{${info.name}} & \\text{${info.description}} \\\\"
                  }, "\n")}
        |      \\end{array}\\) \\\\
       """.stripMargin
    }
    s"""
      |\\subsubsection{\\lst{$opName} method (Code $opCode)}
      |\\label{$label}
      |\\noindent
      |\\begin{tabularx}{\\textwidth}{| l | X |}
      |   \\hline
      |   \\bf{Description} & $desc \\\\
      |   \\hline
      |   \\bf{Signature} & $signature \\\\
      |  $params
      |  \\hline
      |  $serializedAs
      |\\end{tabularx}
      |""".stripMargin
  }

  def printMethods(tc: STypeCompanion) = {
    val nonFrontEndMethods = tc.methods.filter(m => m.docInfo.exists(_.opDesc.isDefined))
    val methodSubsections = for { m <- nonFrontEndMethods.sortBy(_.methodId)} yield {
      methodSubsection(tc, m)
    }
    val res = methodSubsections.mkString("\n\n")
    res
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
    val ops = collectSerializableOperations()
    val noOps = Set(
      TaggedVariable, ValUse, ConstantPlaceholder, TrueLeaf, FalseLeaf,
      ConcreteCollection, ConcreteCollectionBooleanConstant, Tuple, SelectField, SigmaPropIsProven, ValDef, FunDef, BlockValue
    )

    // join collection of all operations with all methods by optional opCode
    val primOps = CollectionUtil.outerJoinSeqs(ops, methods)(
      o => Some(o._1), m => m.docInfo.map(info => if (info.isFrontendOnly) {
        System.err.println(s"WARNING: Operation is frontend only: $info")
        None
      } else info.opDesc.map(_.opCode))
    )(
      (k, o) => Some(o), // left without right
      (k,i) => None,     // right without left
      (k,i,o) => None    // left and right
    ).map(_._2).collect { case Some(op) if !noOps.contains(op._2) => op }

    // primOps is collection of operations which are not referenced by any method m.irInfo.opDesc
    for (p <- primOps) {
      println(s"$p")
    }

    println(s"Total ops: ${primOps.size}\n")


    // join collection of operations with all predef functions by opCode
    val danglingOps = CollectionUtil.outerJoinSeqs(primOps, predefFuncs)(
      o => Some(o._1), f => Some(f.docInfo.opDesc.map(_.opCode))
    )(
      (k, o) => Some(o), // left without right
      (k,i) => None,     // right without left
      (k,i,o) => None    // left and right
    ).map(_._2).collect { case Some(op) => op }

    // danglingOps are the operations which are not referenced by any predef funcs
    for (p <- danglingOps) {
      println(s"$p")
    }

    println(s"Total dangling: ${danglingOps.size}")
  }
}



