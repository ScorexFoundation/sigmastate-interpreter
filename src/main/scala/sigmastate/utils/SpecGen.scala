package sigmastate.utils

import sigmastate._
import sigmastate.eval.Evaluation._
import sigmastate.eval.{Zero, Sized, Evaluation}
import sigma.util.Extensions.ByteOps
import SType._
import scalan.util.{CollectionUtil, FileUtil}
import scalan.meta.PrintExtensions._
import sigmastate.Values.{FalseLeaf, Constant, TrueLeaf, BlockValue, ConstantPlaceholder, Tuple, ValDef, FunDef, ValUse, ValueCompanion, TaggedVariable, ConcreteCollection, ConcreteCollectionBooleanConstant}
import sigmastate.lang.SigmaPredef.PredefinedFuncRegistry
import sigmastate.lang.StdSigmaBuilder
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{ValueSerializer, OpCodes}
import sigmastate.utxo.{SigmaPropIsProven, SelectField}

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

  def collectSerializableOperations(): Seq[(OpCode, ValueCompanion)] = {
    val sers = collectSerializers()
    sers.map(s => (s.opCode, s.opDesc))
  }

  private val predefFuncRegistry = new PredefinedFuncRegistry(StdSigmaBuilder)
  val noFuncs: Set[ValueCompanion] = Set(Constant)
  val predefFuncs = predefFuncRegistry.funcs.filterNot(f => noFuncs.contains(f.info.opDesc))

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
      Range(0, ts.length).map(i => ArgInfo("arg" + i, "")))(info => info.args.toIndexedSeq)
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
    val ops = collectSerializableOperations()
    val noOps = Set(
      TaggedVariable, ValUse, ConstantPlaceholder, TrueLeaf, FalseLeaf,
      ConcreteCollection, ConcreteCollectionBooleanConstant, Tuple, SelectField, SigmaPropIsProven, ValDef, FunDef, BlockValue
    )

    // join collection of all operations with all methods by optional opCode
    val primOps = CollectionUtil.outerJoinSeqs(ops, methods)(
      o => Some(o._1), m => m.irInfo.opDesc.map(_.opCode)
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
      o => Some(o._1), f => Some(f.info.opDesc.opCode)
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

object GenSerializableOps extends SpecGen {
  def collectOpsTable() = {
    val ops = collectSerializableOperations()
    val methods = collectMethods()
    val funcs = predefFuncs

    val methodsByOpCode = methods
      .groupBy(_.irInfo.opDesc.map(_.opCode))
//      .map { case p @ (k, xs) => p.ensuring({ k.isEmpty || xs.length == 1}, p) }

    val funcsByOpCode = funcs
      .groupBy(_.info.opDesc.opCode)
      .ensuring(g => g.forall{ case (k, xs) => xs.length <= 1})

    val table = ops.map { case (opCode, opDesc) =>
      val methodOpt = methodsByOpCode.get(Some(opCode)).map(_.head)
      val funcOpt = funcsByOpCode.get(opCode).map(_.head)
      val m = methodOpt.opt(m => m.objType.typeName + "." + m.name)
      val f = funcOpt.opt(f => f.name)
      (opCode, opDesc, m, f)
    }
    table
  }

  def main(args: Array[String]) = {
    val table = collectOpsTable()
    for (r <- table) {
      println(r)
    }
  }

}

