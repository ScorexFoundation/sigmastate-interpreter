package sigma

import org.ergoplatform.ErgoHeader
import scorex.util.encode.Base16
import sigma.VersionContext.V6SoftForkVersion
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.Token
import scorex.util.ModifierId
import scorex.utils.{Ints, Longs, Shorts}
import sigma.ast.ErgoTree.ZeroHeader
import sigma.ast.SCollection.SByteArray
import sigma.ast.SType.tT
import sigma.ast.syntax.TrueSigmaProp
import sigma.ast.{SInt, _}
import sigma.crypto.SecP256K1Group
import sigma.data.{CBigInt, CBox, CGroupElement, CHeader, CSigmaDslBuilder, ExactNumeric, RType}
import sigma.eval.{CostDetails, SigmaDsl, TracedCost}
import sigma.serialization.ValueCodes.OpCode
import sigma.util.Extensions.{BooleanOps, IntOps}
import sigmastate.exceptions.MethodNotFound
import sigmastate.utils.Extensions.ByteOpsForSigma
import sigmastate.utils.Helpers

import java.math.BigInteger
import scala.util.{Failure, Success}

/** This suite tests all operations for v6.0 version of the language.
  * The base classes establish the infrastructure for the tests.
  *
  * @see SigmaDslSpecificationBase
  */
class LanguageSpecificationV6 extends LanguageSpecificationBase { suite =>
  override def languageVersion: Byte = VersionContext.V6SoftForkVersion

  implicit override def evalSettings = super.evalSettings.copy(printTestVectors = true)

  def mkSerializeFeature[A: RType]: Feature[A, Coll[Byte]] = {
    val tA = RType[A]
    val tpe = Evaluation.rtypeToSType(tA)
    newFeature(
      (x: A) => SigmaDsl.serialize(x),
      s"{ (x: ${tA.name}) => serialize(x) }",
      expectedExpr = FuncValue(
        Array((1, tpe)),
        MethodCall(
          Global,
          SGlobalMethods.serializeMethod.withConcreteTypes(Map(STypeVar("T") -> tpe)),
          Array(ValUse(1, tpe)),
          Map()
        )
      ),
      sinceVersion = VersionContext.V6SoftForkVersion)
  }

  val baseTrace = Array(
    FixedCostItem(Apply),
    FixedCostItem(FuncValue),
    FixedCostItem(GetVar),
    FixedCostItem(OptionGet),
    FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5)))
  )

  property("Global.serialize[Byte]") {
    lazy val serializeByte = mkSerializeFeature[Byte]
    val expectedCostTrace = TracedCost(
      baseTrace ++ Array(
        FixedCostItem(Global),
        FixedCostItem(MethodCall),
        FixedCostItem(ValUse),
        FixedCostItem(NamedDesc("SigmaByteWriter.startWriter"), FixedCost(JitCost(10))),
        FixedCostItem(NamedDesc("SigmaByteWriter.put"), FixedCost(JitCost(1)))
      )
    )
    val cases = Seq(
      (-128.toByte, Expected(Success(Coll(-128.toByte)), expectedCostTrace)),
      (-1.toByte, Expected(Success(Coll(-1.toByte)), expectedCostTrace)),
      (0.toByte, Expected(Success(Coll(0.toByte)), expectedCostTrace)),
      (1.toByte, Expected(Success(Coll(1.toByte)), expectedCostTrace)),
      (127.toByte, Expected(Success(Coll(127.toByte)), expectedCostTrace))
    )
    verifyCases(cases, serializeByte, preGeneratedSamples = None)
  }

  property("Global.serialize[Short]") {
    lazy val serializeShort = mkSerializeFeature[Short]
    val expectedCostTrace = TracedCost(
      baseTrace ++ Array(
        FixedCostItem(Global),
        FixedCostItem(MethodCall),
        FixedCostItem(ValUse),
        FixedCostItem(NamedDesc("SigmaByteWriter.startWriter"), FixedCost(JitCost(10))),
        FixedCostItem(NamedDesc("SigmaByteWriter.putNumeric"), FixedCost(JitCost(3)))
      )
    )
    val cases = Seq(
      (Short.MinValue, Expected(Success(Coll[Byte](0xFF.toByte, 0xFF.toByte, 0x03.toByte)), expectedCostTrace)),
      (-1.toShort, Expected(Success(Coll(1.toByte)), expectedCostTrace)),
      (0.toShort, Expected(Success(Coll(0.toByte)), expectedCostTrace)),
      (1.toShort, Expected(Success(Coll(2.toByte)), expectedCostTrace)),
      (Short.MaxValue, Expected(Success(Coll(-2.toByte, -1.toByte, 3.toByte)), expectedCostTrace))
    )
    verifyCases(cases, serializeShort, preGeneratedSamples = None)
  }


  property("Boolean.toByte") {
    val toByte = newFeature((x: Boolean) => x.toByte, "{ (x: Boolean) => x.toByte }",
      sinceVersion = V6SoftForkVersion
    )

    val cases = Seq(
      (true, Success(1.toByte)),
      (false, Success(0.toByte))
    )

    if (toByte.isSupportedIn(VersionContext.current)) {
      // TODO v6.0: implement as part of https://github.com/ScorexFoundation/sigmastate-interpreter/pull/932
      assertExceptionThrown(
        testCases(cases, toByte),
        rootCauseLike[MethodNotFound]("Cannot find method")
      )
    }
    else
      testCases(cases, toByte)
  }

  property("Byte methods - 6.0 features") {

    lazy val bitOr = newFeature(
      { (x: (Byte, Byte)) => (x._1 | x._2).toByteExact },
      "{ (x: (Byte, Byte)) => x._1.bitwiseOr(x._2) }",
      FuncValue(
        Array((1, SPair(SByte, SByte))),
        MethodCall.typed[Value[SByte.type]](
          SelectField.typed[Value[SByte.type]](ValUse(1, SPair(SByte, SByte)), 1.toByte),
          SByteMethods.v6Methods.find(_.name == "bitwiseOr").get,
          Vector(SelectField.typed[Value[SByte.type]](ValUse(1, SPair(SByte, SByte)),2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (1.toByte, 2.toByte) -> new Expected(ExpectedResult(Success(3.toByte), None))
      ),
      bitOr
    )

    lazy val bitNot = newFeature(
      { (x: Byte) => (~x).toByteExact },
      "{ (x: Byte) => x.bitwiseInverse }",
      FuncValue(
        Array((1, SByte)),
        MethodCall.typed[Value[SByte.type]](
          ValUse(1, SByte),
          SByteMethods.v6Methods.find(_.name == "bitwiseInverse").get,
          Vector(),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        1.toByte -> new Expected(ExpectedResult(Success((-2).toByte), None))
      ),
      bitNot
    )

    lazy val bitAnd = newFeature(
      { (x: (Byte, Byte)) => (x._1 & x._2).toByteExact },
      "{ (x: (Byte, Byte)) => x._1.bitwiseAnd(x._2) }",
      FuncValue(
        Array((1, SPair(SByte, SByte))),
        MethodCall.typed[Value[SByte.type]](
          SelectField.typed[Value[SByte.type]](ValUse(1, SPair(SByte, SByte)), 1.toByte),
          SByteMethods.v6Methods.find(_.name == "bitwiseAnd").get,
          Vector(SelectField.typed[Value[SByte.type]](ValUse(1, SPair(SByte, SByte)),2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (3.toByte, 5.toByte) -> new Expected(ExpectedResult(Success(1.toByte), None))
      ),
      bitAnd
    )

    lazy val bitXor = newFeature(
      { (x: (Byte, Byte)) => (x._1 ^ x._2).toByteExact },
      "{ (x: (Byte, Byte)) => x._1.bitwiseXor(x._2) }",
      FuncValue(
        Array((1, SPair(SByte, SByte))),
        MethodCall.typed[Value[SByte.type]](
          SelectField.typed[Value[SByte.type]](ValUse(1, SPair(SByte, SByte)), 1.toByte),
          SByteMethods.v6Methods.find(_.name == "bitwiseXor").get,
          Vector(SelectField.typed[Value[SByte.type]](ValUse(1, SPair(SByte, SByte)),2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (3.toByte, 5.toByte) -> new Expected(ExpectedResult(Success(6.toByte), None))
      ),
      bitXor
    )

    lazy val toBigEndianBytes = newFeature(
      { x: Byte => Coll(x) },
      "{ (x: Byte) => x.toBytes }",
      FuncValue(
        Array((1, SByte)),
        MethodCall.typed[Value[SCollection[SByte.type]]](
          ValUse(1, SByte),
          SByteMethods.getMethodByName("toBytes"),
          Vector(),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        127.toByte -> new Expected(ExpectedResult(Success(Coll(127.toByte)), None))
      ),
      toBigEndianBytes
    )

    def byte2Bools(b: Byte): Seq[Boolean] =
      (0 to 7 map isBitSet(b)).reverse

    def isBitSet(byte: Byte)(bit: Int): Boolean =
      ((byte >> bit) & 1) == 1

    lazy val toBits = newFeature[Byte, Coll[Boolean]](
      { x: Byte => Colls.fromArray(byte2Bools(x).toArray) },
      "{ (x: Byte) => x.toBits }",
      FuncValue(
        Array((1, SByte)),
        MethodCall.typed[Value[SCollection[SByte.type]]](
          ValUse(1, SByte),
          SByteMethods.getMethodByName("toBits"),
          Vector(),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        83.toByte -> new Expected(ExpectedResult(Success(Coll(false, true, false, true, false, false, true, true)), None)),
        -55.toByte -> new Expected(ExpectedResult(Success(Coll(true, true, false, false, true, false, false, true)), None)),
        -1.toByte -> new Expected(ExpectedResult(Success(Coll(true, true, true, true, true, true, true, true)), None))
      ),
      toBits
    )

    lazy val shiftLeft = newFeature(
      { (x: (Byte, Int)) => if(x._2 < 0 || x._2 >= 8) throw new IllegalArgumentException() else (x._1 << x._2).toByte },
      "{ (x: (Byte, Int)) => x._1.shiftLeft(x._2) }",
      FuncValue(
        Array((1, SPair(SByte, SInt))),
        MethodCall.typed[Value[SByte.type]](
          SelectField.typed[Value[SByte.type]](ValUse(1, SPair(SByte, SInt)), 1.toByte),
          SByteMethods.v6Methods.find(_.name == "shiftLeft").get,
          Vector(SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SByte, SInt)), 2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (3.toByte, 3) -> new Expected(ExpectedResult(Success(24.toByte), None)),
        (3.toByte, 0) -> new Expected(ExpectedResult(Success(3.toByte), None)),
        (3.toByte, -1) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None)),
        (3.toByte, 8) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None))
      ),
      shiftLeft,
      preGeneratedSamples = Some(Seq())
    )

    lazy val shiftRight = newFeature(
      { (x: (Byte, Int)) => if(x._2 < 0 || x._2 >= 8) throw new IllegalArgumentException() else (x._1 >> x._2).toByte },
      "{ (x: (Byte, Int)) => x._1.shiftRight(x._2) }",
      FuncValue(
        Array((1, SPair(SByte, SInt))),
        MethodCall.typed[Value[SByte.type]](
          SelectField.typed[Value[SByte.type]](ValUse(1, SPair(SByte, SInt)), 1.toByte),
          SByteMethods.v6Methods.find(_.name == "shiftRight").get,
          Vector(SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SByte, SInt)), 2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (24.toByte, 3) -> new Expected(ExpectedResult(Success(3.toByte), None)),
        (24.toByte, 0) -> new Expected(ExpectedResult(Success(24.toByte), None)),
        (24.toByte, -1) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None)),
        (24.toByte, 8) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None))
      ),
      shiftRight,
      preGeneratedSamples = Some(Seq())
    )
  }

  property("Short - 6.0 methods") {

    lazy val bitOr = newFeature(
      { (x: (Short, Short)) => (x._1 | x._2).toShortExact },
      "{ (x: (Short, Short)) => x._1.bitwiseOr(x._2) }",
      FuncValue(
        Array((1, SPair(SShort, SShort))),
        MethodCall.typed[Value[SShort.type]](
          SelectField.typed[Value[SShort.type]](ValUse(1,SPair(SShort, SShort)), 1.toByte),
          SShortMethods.v6Methods.find(_.name == "bitwiseOr").get,
          Vector(SelectField.typed[Value[SShort.type]](ValUse(1, SPair(SShort, SShort)),2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (1.toShort, 2.toShort) -> new Expected(ExpectedResult(Success(3.toShort), None)),
        (1001.toShort, 2002.toShort) -> new Expected(ExpectedResult(Success(2043.toShort), None))
      ),
      bitOr
    )

    lazy val bitNot = newFeature(
      { (x: Short) => (~x).toShortExact },
      "{ (x: Short) => x.bitwiseInverse }",
      FuncValue(
        Array((1, SShort)),
        MethodCall.typed[Value[SShort.type]](
          ValUse(1, SShort),
          SShortMethods.v6Methods.find(_.name == "bitwiseInverse").get,
          Vector(),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        1.toShort -> new Expected(ExpectedResult(Success((-2).toShort), None)),
        10001.toShort -> new Expected(ExpectedResult(Success((-10002).toShort), None))
      ),
      bitNot
    )

    lazy val bitAnd = newFeature(
      { (x: (Short, Short)) => (x._1 & x._2).toShortExact },
      "{ (x: (Short, Short)) => x._1.bitwiseAnd(x._2) }",
      FuncValue(
        Array((1, SPair(SShort, SShort))),
        MethodCall.typed[Value[SShort.type]](
          SelectField.typed[Value[SShort.type]](ValUse(1, SPair(SShort, SShort)), 1.toByte),
          SShortMethods.v6Methods.find(_.name == "bitwiseAnd").get,
          Vector(SelectField.typed[Value[SShort.type]](ValUse(1, SPair(SShort, SShort)),2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (3.toShort, 5.toShort) -> new Expected(ExpectedResult(Success(1.toShort), None)),
        (10001.toShort, 2202.toShort) -> new Expected(ExpectedResult(Success(16.toShort), None))
      ),
      bitAnd
    )

    lazy val bitXor = newFeature(
      { (x: (Short, Short)) => (x._1 ^ x._2).toShortExact },
      "{ (x: (Short, Short)) => x._1.bitwiseXor(x._2) }",
      FuncValue(
        Array((1, SPair(SShort, SShort))),
        MethodCall.typed[Value[SShort.type]](
          SelectField.typed[Value[SShort.type]](ValUse(1, SPair(SShort, SShort)), 1.toByte),
          SShortMethods.v6Methods.find(_.name == "bitwiseXor").get,
          Vector(SelectField.typed[Value[SShort.type]](ValUse(1, SPair(SShort, SShort)),2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (3.toShort, 5.toShort) -> new Expected(ExpectedResult(Success(6.toShort), None)),
        (10001.toShort, 2202.toShort) -> new Expected(ExpectedResult(Success(12171.toShort), None))
      ),
      bitXor
    )

    lazy val toBigEndianBytes = newFeature[Short, Coll[Byte]](
      { x: Short => Colls.fromArray(Shorts.toByteArray(x)) },
      "{ (x: Short) => x.toBytes }",
      FuncValue(
        Array((1, SShort)),
        MethodCall.typed[Value[SCollection[SShort.type]]](
          ValUse(1, SShort),
          SShortMethods.getMethodByName("toBytes"),
          Vector(),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        127.toShort -> new Expected(ExpectedResult(Success(Coll(0.toByte, 127.toByte)), None)),
        Short.MaxValue -> new Expected(ExpectedResult(Success(Coll(127.toByte, (-1).toByte)), None)),
        Short.MinValue -> new Expected(ExpectedResult(Success(Coll((-128).toByte, 0.toByte)), None))
      ),
      toBigEndianBytes
    )

    def byte2Bools(b: Byte): Seq[Boolean] =
      (0 to 7 map isBitSet(b)).reverse

    def isBitSet(byte: Byte)(bit: Int): Boolean =
      ((byte >> bit) & 1) == 1

    lazy val toBits = newFeature[Short, Coll[Boolean]](
      { x: Short => Colls.fromArray(Shorts.toByteArray(x)).flatMap(b => Colls.fromArray(byte2Bools(b).toArray)) },
      "{ (x: Short) => x.toBits }",
      FuncValue(
        Array((1, SShort)),
        MethodCall.typed[Value[SCollection[SShort.type]]](
          ValUse(1, SShort),
          SShortMethods.getMethodByName("toBits"),
          Vector(),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        83.toShort -> new Expected(ExpectedResult(Success(Coll(false, false, false, false, false, false, false, false, false, true, false, true, false, false, true, true)), None)),
        -55.toShort -> new Expected(ExpectedResult(Success(Coll(true, true, true, true, true, true, true, true, true, true, false, false, true, false, false, true)), None)),
        -1.toShort-> new Expected(ExpectedResult(Success(Coll(true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true)), None)),
        -10001.toShort-> new Expected(ExpectedResult(Success(Coll(true, true, false, true, true, false, false, false, true, true, true, false, true, true, true, true)), None))
      ),
      toBits
    )

    lazy val shiftLeft = newFeature(
      { (x: (Short, Int)) => if(x._2 < 0 || x._2 >= 16) throw new IllegalArgumentException() else (x._1 << x._2).toShort },
      "{ (x: (Short, Int)) => x._1.shiftLeft(x._2) }",
      FuncValue(
        Array((1, SPair(SShort, SInt))),
        MethodCall.typed[Value[SShort.type]](
          SelectField.typed[Value[SShort.type]](ValUse(1, SPair(SShort, SInt)), 1.toByte),
          SShortMethods.v6Methods.find(_.name == "shiftLeft").get,
          Vector(SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SShort, SInt)), 2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (3.toShort, 3) -> new Expected(ExpectedResult(Success(24.toShort), None)),
        (3.toShort, 8) -> new Expected(ExpectedResult(Success(768.toShort), None)),
        ((-2).toShort, 10) -> new Expected(ExpectedResult(Success((-2048).toShort), None)),
        ((-2).toShort, 20) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None)),
        (3.toShort, -1) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None))
      ),
      shiftLeft,
      preGeneratedSamples = Some(Seq())
    )

    lazy val shiftRight = newFeature(
      { (x: (Short, Int)) => if(x._2 < 0 || x._2 >= 16) throw new IllegalArgumentException() else (x._1 >> x._2).toShort },
      "{ (x: (Short, Int)) => x._1.shiftRight(x._2) }",
      FuncValue(
        Array((1, SPair(SShort, SInt))),
        MethodCall.typed[Value[SShort.type]](
          SelectField.typed[Value[SShort.type]](ValUse(1, SPair(SShort, SInt)), 1.toByte),
          SShortMethods.v6Methods.find(_.name == "shiftRight").get,
          Vector(SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SShort, SInt)), 2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (24.toShort, 3) -> new Expected(ExpectedResult(Success(3.toShort), None)),
        (1600.toShort, 8) -> new Expected(ExpectedResult(Success(6.toShort), None)),
        ((-3200).toShort, 8) -> new Expected(ExpectedResult(Success((-13).toShort), None)),
        (3.toShort, -1) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None)),
        (3.toShort, 16) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None))
      ),
      shiftRight,
      preGeneratedSamples = Some(Seq())
    )
  }

  property("Int - 6.0 methods") {

    lazy val bitOr = newFeature(
      { (x: (Int, Int)) => (x._1 | x._2)},
      "{ (x: (Int, Int)) => x._1.bitwiseOr(x._2) }",
      FuncValue(
        Array((1, SPair(SInt, SInt))),
        MethodCall.typed[Value[SInt.type]](
          SelectField.typed[Value[SInt.type]](ValUse(1,SPair(SInt, SInt)), 1.toByte),
          SIntMethods.v6Methods.find(_.name == "bitwiseOr").get,
          Vector(SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SInt, SInt)),2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (1, 2) -> new Expected(ExpectedResult(Success(3), None)),
        (1001, 2002) -> new Expected(ExpectedResult(Success(2043), None)),
        (100001, 20002) -> new Expected(ExpectedResult(Success(118435), None))
      ),
      bitOr
    )

    lazy val bitNot = newFeature(
      { (x: Int) => ~x },
      "{ (x: Int) => x.bitwiseInverse }",
      FuncValue(
        Array((1, SInt)),
        MethodCall.typed[Value[SInt.type]](
          ValUse(1, SInt),
          SIntMethods.v6Methods.find(_.name == "bitwiseInverse").get,
          Vector(),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        1 -> new Expected(ExpectedResult(Success(-2), None)),
        10001 -> new Expected(ExpectedResult(Success(-10002), None)),
        Int.MinValue -> new Expected(ExpectedResult(Success(Int.MaxValue), None))
      ),
      bitNot
    )

    lazy val bitAnd = newFeature(
      { (x: (Int, Int)) => x._1 & x._2 },
      "{ (x: (Int, Int)) => x._1.bitwiseAnd(x._2) }",
      FuncValue(
        Array((1, SPair(SInt, SInt))),
        MethodCall.typed[Value[SInt.type]](
          SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SInt, SInt)), 1.toByte),
          SIntMethods.v6Methods.find(_.name == "bitwiseAnd").get,
          Vector(SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SInt, SInt)),2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (3, 5) -> new Expected(ExpectedResult(Success(1), None)),
        (10001, 2202) -> new Expected(ExpectedResult(Success(16), None)),
        (-10001, 200202) -> new Expected(ExpectedResult(Success(198666), None))
      ),
      bitAnd
    )

    lazy val bitXor = newFeature(
      { (x: (Int, Int)) => (x._1 ^ x._2) },
      "{ (x: (Int, Int)) => x._1.bitwiseXor(x._2) }",
      FuncValue(
        Array((1, SPair(SInt, SInt))),
        MethodCall.typed[Value[SInt.type]](
          SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SInt, SInt)), 1.toByte),
          SIntMethods.v6Methods.find(_.name == "bitwiseXor").get,
          Vector(SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SInt, SInt)),2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (3, 5) -> new Expected(ExpectedResult(Success(6), None)),
        (10001, 2202) -> new Expected(ExpectedResult(Success(12171), None)),
        (-10001, 200202) -> new Expected(ExpectedResult(Success(-207131), None))
      ),
      bitXor
    )

    lazy val toBigEndianBytes = newFeature[Int, Coll[Byte]](
      { x: Int => Colls.fromArray(Ints.toByteArray(x)) },
      "{ (x: Int) => x.toBytes }",
      FuncValue(
        Array((1, SInt)),
        MethodCall.typed[Value[SCollection[SInt.type]]](
          ValUse(1, SInt),
          SIntMethods.getMethodByName("toBytes"),
          Vector(),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        127 -> new Expected(ExpectedResult(Success(Coll(0.toByte, 0.toByte, 0.toByte, 127.toByte)), None)),
        Short.MaxValue.toInt -> new Expected(ExpectedResult(Success(Coll(0.toByte, 0.toByte, 127.toByte, (-1).toByte)), None)),
        Short.MinValue.toInt -> new Expected(ExpectedResult(Success(Coll((-1).toByte, (-1).toByte, (-128).toByte, 0.toByte)), None)),
        Int.MaxValue.toInt -> new Expected(ExpectedResult(Success(Coll(127.toByte, (-1).toByte, (-1).toByte, (-1).toByte)), None))
      ),
      toBigEndianBytes
    )

    def byte2Bools(b: Byte): Seq[Boolean] =
      (0 to 7 map isBitSet(b)).reverse

    def isBitSet(byte: Byte)(bit: Int): Boolean =
      ((byte >> bit) & 1) == 1

    lazy val toBits = newFeature[Int, Coll[Boolean]](
      { x: Int => Colls.fromArray(Ints.toByteArray(x)).flatMap(b => Colls.fromArray(byte2Bools(b).toArray))  },
      "{ (x: Int) => x.toBits }",
      FuncValue(
        Array((1, SInt)),
        MethodCall.typed[Value[SCollection[SInt.type]]](
          ValUse(1, SInt),
          SIntMethods.getMethodByName("toBits"),
          Vector(),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        83 -> new Expected(ExpectedResult(Success(Coll(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, true, false, false, true, true)), None)),
        -55 -> new Expected(ExpectedResult(Success(Coll(true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, false, true, false, false, true)), None)),
        -1 -> new Expected(ExpectedResult(Success(Coll(true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true)), None)),
        -10001 -> new Expected(ExpectedResult(Success(Coll(true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, false, true, true, false, false, false, true, true, true, false, true, true, true, true)), None))
      ),
      toBits
    )

    lazy val shiftLeft = newFeature(
      { (x: (Int, Int)) => if(x._2 < 0 || x._2 >= 32) throw new IllegalArgumentException() else (x._1 << x._2) },
      "{ (x: (Int, Int)) => x._1.shiftLeft(x._2) }",
      FuncValue(
        Array((1, SPair(SInt, SInt))),
        MethodCall.typed[Value[SInt.type]](
          SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SInt, SInt)), 1.toByte),
          SIntMethods.v6Methods.find(_.name == "shiftLeft").get,
          Vector(SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SInt, SInt)), 2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (3, 3) -> new Expected(ExpectedResult(Success(24), None)),
        (3, 8) -> new Expected(ExpectedResult(Success(768), None)),
        (-2, 10) -> new Expected(ExpectedResult(Success(-2048), None)),
        (-222, 10) -> new Expected(ExpectedResult(Success(-227328), None)),
        (-222, 32) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None)),
        (-222, -1) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None))
      ),
      shiftLeft,
      preGeneratedSamples = Some(Seq())
    )

    lazy val shiftRight = newFeature(
      { (x: (Int, Int)) => if(x._2 < 0 || x._2 >= 32) throw new IllegalArgumentException() else (x._1 >> x._2) },
      "{ (x: (Int, Int)) => x._1.shiftRight(x._2) }",
      FuncValue(
        Array((1, SPair(SInt, SInt))),
        MethodCall.typed[Value[SInt.type]](
          SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SInt, SInt)), 1.toByte),
          SIntMethods.v6Methods.find(_.name == "shiftRight").get,
          Vector(SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SInt, SInt)), 2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (24, 3) -> new Expected(ExpectedResult(Success(3), None)),
        (1600, 8) -> new Expected(ExpectedResult(Success(6), None)),
        (-3200, 8) -> new Expected(ExpectedResult(Success(-13), None)),
        (-320019, 18) -> new Expected(ExpectedResult(Success(-2), None)),
        (-320019, 32) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None)),
        (-320019, -1) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None))
      ),
      shiftRight,
      preGeneratedSamples = Some(Seq())
    )
  }

  property("Long - 6.0 methods") {

    lazy val bitOr = newFeature(
      { (x: (Long, Long)) => (x._1 | x._2)},
      "{ (x: (Long, Long)) => x._1.bitwiseOr(x._2) }",
      FuncValue(
        Array((1, SPair(SLong, SLong))),
        MethodCall.typed[Value[SLong.type]](
          SelectField.typed[Value[SLong.type]](ValUse(1,SPair(SLong, SLong)), 1.toByte),
          SLongMethods.v6Methods.find(_.name == "bitwiseOr").get,
          Vector(SelectField.typed[Value[SLong.type]](ValUse(1, SPair(SLong, SLong)),2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (1L, 2L) -> new Expected(ExpectedResult(Success(3L), None)),
        (1001L, 2002L) -> new Expected(ExpectedResult(Success(2043L), None)),
        (100001L, 20002L) -> new Expected(ExpectedResult(Success(118435L), None)),
        (1000010111L, -22L) -> new Expected(ExpectedResult(Success(-1L), None))
      ),
      bitOr
    )

    lazy val bitNot = newFeature(
      { (x: Long) => ~x },
      "{ (x: Long) => x.bitwiseInverse }",
      FuncValue(
        Array((1, SLong)),
        MethodCall.typed[Value[SLong.type]](
          ValUse(1, SLong),
          SLongMethods.v6Methods.find(_.name == "bitwiseInverse").get,
          Vector(),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        1L -> new Expected(ExpectedResult(Success(-2L), None)),
        10001L -> new Expected(ExpectedResult(Success(-10002L), None)),
        Int.MinValue.toLong -> new Expected(ExpectedResult(Success(Int.MaxValue.toLong), None)),
        Long.MinValue -> new Expected(ExpectedResult(Success(Long.MaxValue), None)),
        Long.MaxValue -> new Expected(ExpectedResult(Success(Long.MinValue), None))
      ),
      bitNot
    )

    lazy val bitAnd = newFeature(
      { (x: (Long, Long)) => x._1 & x._2 },
      "{ (x: (Long, Long)) => x._1.bitwiseAnd(x._2) }",
      FuncValue(
        Array((1, SPair(SLong, SLong))),
        MethodCall.typed[Value[SLong.type]](
          SelectField.typed[Value[SLong.type]](ValUse(1, SPair(SLong, SLong)), 1.toByte),
          SLongMethods.v6Methods.find(_.name == "bitwiseAnd").get,
          Vector(SelectField.typed[Value[SLong.type]](ValUse(1, SPair(SLong, SLong)),2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (3L, 5L) -> new Expected(ExpectedResult(Success(1L), None)),
        (10001L, 2202L) -> new Expected(ExpectedResult(Success(16L), None)),
        (-10001L, 200202L) -> new Expected(ExpectedResult(Success(198666L), None)),
        (1000010111L, -22L) -> new Expected(ExpectedResult(Success(1000010090L), None))
      ),
      bitAnd
    )

    lazy val bitXor = newFeature(
      { (x: (Long, Long)) => (x._1 ^ x._2) },
      "{ (x: (Long, Long)) => x._1.bitwiseXor(x._2) }",
      FuncValue(
        Array((1, SPair(SLong, SLong))),
        MethodCall.typed[Value[SLong.type]](
          SelectField.typed[Value[SLong.type]](ValUse(1, SPair(SLong, SLong)), 1.toByte),
          SLongMethods.v6Methods.find(_.name == "bitwiseXor").get,
          Vector(SelectField.typed[Value[SLong.type]](ValUse(1, SPair(SLong, SLong)),2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (3L, 5L) -> new Expected(ExpectedResult(Success(6L), None)),
        (10001L, 2202L) -> new Expected(ExpectedResult(Success(12171L), None)),
        (-10001L, 200202L) -> new Expected(ExpectedResult(Success(-207131L), None)),
        (1000010111L, -22L) -> new Expected(ExpectedResult(Success(-1000010091L), None))
      ),
      bitXor
    )

    lazy val toBigEndianBytes = newFeature[Long, Coll[Byte]](
      { x: Long => Colls.fromArray(Longs.toByteArray(x)) },
      "{ (x: Long) => x.toBytes }",
      FuncValue(
        Array((1, SLong)),
        MethodCall.typed[Value[SCollection[SLong.type]]](
          ValUse(1, SLong),
          SLongMethods.getMethodByName("toBytes"),
          Vector(),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        127L -> new Expected(ExpectedResult(Success(Coll(0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte, 127.toByte)), None)),
        Short.MaxValue.toLong -> new Expected(ExpectedResult(Success(Coll(0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte, 127.toByte, (-1).toByte)), None)),
        Short.MinValue.toLong -> new Expected(ExpectedResult(Success(Coll((-1).toByte, (-1).toByte, (-1).toByte, (-1).toByte, (-1).toByte, (-1).toByte, (-128).toByte, 0.toByte)), None)),
        Int.MaxValue.toLong -> new Expected(ExpectedResult(Success(Coll(0.toByte, 0.toByte, 0.toByte, 0.toByte, 127.toByte, (-1).toByte, (-1).toByte, (-1).toByte)), None))
      ),
      toBigEndianBytes
    )

    def byte2Bools(b: Byte): Seq[Boolean] =
      (0 to 7 map isBitSet(b)).reverse

    def isBitSet(byte: Byte)(bit: Int): Boolean =
      ((byte >> bit) & 1) == 1

    lazy val toBits = newFeature[Long, Coll[Boolean]](
      { x: Long => Colls.fromArray(Longs.toByteArray(x)).flatMap(b => Colls.fromArray(byte2Bools(b).toArray)) },
      "{ (x: Long) => x.toBits }",
      FuncValue(
        Array((1, SLong)),
        MethodCall.typed[Value[SCollection[SLong.type]]](
          ValUse(1, SLong),
          SLongMethods.getMethodByName("toBits"),
          Vector(),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        83L -> new Expected(ExpectedResult(Success(Colls.fromArray(Array.fill(57)(false)).append(Coll(true, false, true, false, false, true, true))), None)),
        -55L -> new Expected(ExpectedResult(Success(Colls.fromArray(Array.fill(58)(true)).append(Coll(false, false, true, false, false, true))), None)),
        -1L -> new Expected(ExpectedResult(Success(Colls.fromArray(Array.fill(64)(true))), None)),
        -10001L -> new Expected(ExpectedResult(Success(Colls.fromArray(Array.fill(50)(true)).append(Coll( false, true, true, false, false, false, true, true, true, false, true, true, true, true))), None))
      ),
      toBits
    )

    lazy val shiftLeft = newFeature(
      { (x: (Long, Int)) => if(x._2 < 0 || x._2 >= 32) throw new IllegalArgumentException() else (x._1 << x._2) },
      "{ (x: (Long, Int)) => x._1.shiftLeft(x._2) }",
      FuncValue(
        Array((1, SPair(SLong, SInt))),
        MethodCall.typed[Value[SLong.type]](
          SelectField.typed[Value[SLong.type]](ValUse(1, SPair(SLong, SInt)), 1.toByte),
          SLongMethods.v6Methods.find(_.name == "shiftLeft").get,
          Vector(SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SLong, SInt)), 2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (3L, 3) -> new Expected(ExpectedResult(Success(24L), None)),
        (3L, 8) -> new Expected(ExpectedResult(Success(768L), None)),
        (-2L, 10) -> new Expected(ExpectedResult(Success(-2048L), None)),
        (-222L, 10) -> new Expected(ExpectedResult(Success(-227328L), None)),
        (-222L, -1) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None)),
        (-222L, 64) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None))
      ),
      shiftLeft,
      preGeneratedSamples = Some(Seq())
    )

    lazy val shiftRight = newFeature(
      { (x: (Long, Int)) => if(x._2 < 0 || x._2 >= 64) throw new IllegalArgumentException() else (x._1 >> x._2) },
      "{ (x: (Long, Int)) => x._1.shiftRight(x._2) }",
      FuncValue(
        Array((1, SPair(SLong, SInt))),
        MethodCall.typed[Value[SLong.type]](
          SelectField.typed[Value[SLong.type]](ValUse(1, SPair(SLong, SInt)), 1.toByte),
          SLongMethods.v6Methods.find(_.name == "shiftRight").get,
          Vector(SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SLong, SInt)), 2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (24L, 3) -> new Expected(ExpectedResult(Success(3L), None)),
        (1600L, 8) -> new Expected(ExpectedResult(Success(6L), None)),
        (-3200L, 8) -> new Expected(ExpectedResult(Success(-13L), None)),
        (-320019L, 18) -> new Expected(ExpectedResult(Success(-2L), None)),
        (-320019L, 63) -> new Expected(ExpectedResult(Success(-1L), None)),
        (24L, -1) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None))
      ),
      shiftRight,
      preGeneratedSamples = Some(Seq())
    )
  }

  property("BigInt - 6.0 features") {
    import sigma.data.OrderingOps.BigIntOrdering

    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // The `Upcast(bigInt, SBigInt)` node is never produced by ErgoScript compiler, but is still valid ErgoTree.
      // Fixed in 6.0
      assertExceptionThrown(
        SBigInt.upcast(CBigInt(new BigInteger("0", 16)).asInstanceOf[AnyVal]),
        _.getMessage.contains("Cannot upcast value")
      )

      // The `Downcast(bigInt, SBigInt)` node is never produced by ErgoScript compiler, but is still valid ErgoTree.
      // Fixed in 6.0
      assertExceptionThrown(
        SBigInt.downcast(CBigInt(new BigInteger("0", 16)).asInstanceOf[AnyVal]),
        _.getMessage.contains("Cannot downcast value")
      )

      forAll { x: Long =>
        assertExceptionThrown(
          SLong.downcast(CBigInt(new BigInteger(x.toString)).asInstanceOf[AnyVal]),
          _.getMessage.contains("Cannot downcast value")
        )
      }
      forAll { x: Int =>
        assertExceptionThrown(
          SInt.downcast(CBigInt(new BigInteger(x.toString)).asInstanceOf[AnyVal]),
          _.getMessage.contains("Cannot downcast value")
        )
      }
      forAll { x: Byte =>
        assertExceptionThrown(
          SByte.downcast(CBigInt(new BigInteger(x.toString)).asInstanceOf[AnyVal]),
          _.getMessage.contains("Cannot downcast value")
        )
      }
      forAll { x: Short =>
        assertExceptionThrown(
          SShort.downcast(CBigInt(new BigInteger(x.toString)).asInstanceOf[AnyVal]),
          _.getMessage.contains("Cannot downcast value")
        )
      }
    } else {
      forAll { x: BigInteger =>
        SBigInt.upcast(CBigInt(x).asInstanceOf[AnyVal]) shouldBe CBigInt(x)
        SBigInt.downcast(CBigInt(x).asInstanceOf[AnyVal]) shouldBe CBigInt(x)
      }
      forAll { x: Long =>
          SLong.downcast(CBigInt(new BigInteger(x.toString)).asInstanceOf[AnyVal]) shouldBe x
      }
      forAll { x: Int =>
          SInt.downcast(CBigInt(new BigInteger(x.toString)).asInstanceOf[AnyVal]) shouldBe x
      }
      forAll { x: Byte =>
        SByte.downcast(CBigInt(new BigInteger(x.toString)).asInstanceOf[AnyVal]) shouldBe x
      }
      forAll { x: Short =>
        SShort.downcast(CBigInt(new BigInteger(x.toString)).asInstanceOf[AnyVal]) shouldBe x
      }
    }

    lazy val bitOr = newFeature[(BigInt, BigInt), BigInt](
      { (x: (BigInt, BigInt)) => (x._1 | x._2)},
      "{ (x: (BigInt, BigInt)) => x._1.bitwiseOr(x._2) }",
      FuncValue(
        Array((1, SPair(SBigInt, SBigInt))),
        MethodCall.typed[Value[SBigInt.type]](
          SelectField.typed[Value[SBigInt.type]](ValUse(1,SPair(SBigInt, SBigInt)), 1.toByte),
          SBigIntMethods.v6Methods.find(_.name == "bitwiseOr").get,
          Vector(SelectField.typed[Value[SBigInt.type]](ValUse(1, SPair(SBigInt, SBigInt)),2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (CBigInt(BigInteger.valueOf(1)), CBigInt(BigInteger.valueOf(2))) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(3))), None)),
        (CBigInt(BigInteger.valueOf(1001)), CBigInt(BigInteger.valueOf(2002))) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(2043))), None)),
        (CBigInt(BigInteger.valueOf(100001)), CBigInt(BigInteger.valueOf(20002))) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(118435))), None)),
        (CBigInt(BigInteger.valueOf(1000010111)), CBigInt(BigInteger.valueOf(-22))) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(-1))), None))
      ),
      bitOr
    )

    lazy val bitNot = newFeature[BigInt, BigInt](
      { (x: BigInt) => CBigInt(x.asInstanceOf[CBigInt].wrappedValue.not()) },
      "{ (x: BigInt) => x.bitwiseInverse }",
      FuncValue(
        Array((1, SBigInt)),
        MethodCall.typed[Value[SBigInt.type]](
          ValUse(1, SBigInt),
          SBigIntMethods.v6Methods.find(_.name == "bitwiseInverse").get,
          Vector(),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        CBigInt(BigInteger.valueOf(1)) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(-2))), None)),
        CBigInt(BigInteger.valueOf(10001)) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(-10002))), None)),
        CBigInt(BigInteger.valueOf(Int.MinValue)) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(Int.MaxValue))), None)),
        CBigInt(BigInteger.valueOf(Long.MinValue)) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(Long.MaxValue))), None)),
        CBigInt(BigInteger.valueOf(Long.MaxValue)) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(Long.MinValue))), None))
      ),
      bitNot
    )

    lazy val bitAnd = newFeature(
      { (x: (BigInt, BigInt)) => x._1.asInstanceOf[CBigInt].and(x._2.asInstanceOf[CBigInt]) },
      "{ (x: (BigInt, BigInt)) => x._1.bitwiseAnd(x._2) }",
      FuncValue(
        Array((1, SPair(SBigInt, SBigInt))),
        MethodCall.typed[Value[SBigInt.type]](
          SelectField.typed[Value[SBigInt.type]](ValUse(1, SPair(SBigInt, SBigInt)), 1.toByte),
          SBigIntMethods.v6Methods.find(_.name == "bitwiseAnd").get,
          Vector(SelectField.typed[Value[SBigInt.type]](ValUse(1, SPair(SBigInt, SBigInt)), 2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (CBigInt(BigInteger.valueOf(3)), CBigInt(BigInteger.valueOf(5))) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(1))), None)),
        (CBigInt(BigInteger.valueOf(10001)), CBigInt(BigInteger.valueOf(2202))) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(16))), None)),
        (CBigInt(BigInteger.valueOf(-10001)), CBigInt(BigInteger.valueOf(200202))) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(198666))), None)),
        (CBigInt(BigInteger.valueOf(1000010111)), CBigInt(BigInteger.valueOf(-22))) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(1000010090))), None))
      ),
      bitAnd
    )

    lazy val bitXor = newFeature(
      { (x: (BigInt, BigInt)) => x._1.asInstanceOf[CBigInt].xor(x._2.asInstanceOf[CBigInt]) },
      "{ (x: (BigInt, BigInt)) => x._1.bitwiseXor(x._2) }",
      FuncValue(
        Array((1, SPair(SBigInt, SBigInt))),
        MethodCall.typed[Value[SBigInt.type]](
          SelectField.typed[Value[SBigInt.type]](ValUse(1, SPair(SBigInt, SBigInt)), 1.toByte),
          SBigIntMethods.v6Methods.find(_.name == "bitwiseXor").get,
          Vector(SelectField.typed[Value[SBigInt.type]](ValUse(1, SPair(SBigInt, SBigInt)),2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (CBigInt(BigInteger.valueOf(3)), CBigInt(BigInteger.valueOf(5))) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(6))), None)),
        (CBigInt(BigInteger.valueOf(10001)), CBigInt(BigInteger.valueOf(2202))) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(12171))), None)),
        (CBigInt(BigInteger.valueOf(-10001)), CBigInt(BigInteger.valueOf(200202))) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(-207131))), None)),
        (CBigInt(BigInteger.valueOf(1000010111)), CBigInt(BigInteger.valueOf(-22))) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(-1000010091))), None))
      ),
      bitXor
    )

    lazy val toBigEndianBytes = newFeature[BigInt, Coll[Byte]](
      { x: BigInt => x.toBytes },
      "{ (x: BigInt) => x.toBytes }",
      FuncValue(
        Array((1, SBigInt)),
        MethodCall.typed[Value[SCollection[SBigInt.type]]](
          ValUse(1, SBigInt),
          SBigIntMethods.getMethodByName("toBytes"),
          Vector(),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        CBigInt(BigInteger.valueOf(127)) -> new Expected(ExpectedResult(Success(Coll(127.toByte)), None)),
        CBigInt(BigInteger.valueOf(Short.MaxValue)) -> new Expected(ExpectedResult(Success(Coll(127.toByte, (-1).toByte)), None)),
        CBigInt(BigInteger.valueOf(Short.MinValue)) -> new Expected(ExpectedResult(Success(Coll((-128).toByte, 0.toByte)), None)),
        CBigInt(BigInteger.valueOf(Int.MaxValue)) -> new Expected(ExpectedResult(Success(Coll(127.toByte, (-1).toByte, (-1).toByte, (-1).toByte)), None))
      ),
      toBigEndianBytes
    )

    def byte2Bools(b: Byte): Seq[Boolean] =
      (0 to 7 map isBitSet(b)).reverse

    def isBitSet(byte: Byte)(bit: Int): Boolean =
      ((byte >> bit) & 1) == 1

    lazy val toBits = newFeature[BigInt, Coll[Boolean]](
      { x: BigInt => x.toBytes.flatMap(b => Colls.fromArray(byte2Bools(b).toArray))  },
      "{ (x: BigInt) => x.toBits }",
      FuncValue(
        Array((1, SBigInt)),
        MethodCall.typed[Value[SCollection[SBigInt.type]]](
          ValUse(1, SBigInt),
          SBigIntMethods.getMethodByName("toBits"),
          Vector(),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        CBigInt(BigInteger.valueOf(83)) -> new Expected(ExpectedResult(Success(Coll(false, true, false, true, false, false, true, true)), None)),
        CBigInt(BigInteger.valueOf(-55)) -> new Expected(ExpectedResult(Success(Coll(true, true, false, false, true, false, false, true)), None)),
        CBigInt(BigInteger.valueOf(-1L)) -> new Expected(ExpectedResult(Success(Colls.fromArray(Array.fill(8)(true))), None)),
        CBigInt(BigInteger.valueOf(-10001L)) -> new Expected(ExpectedResult(Success(Coll(true,true,false,true,true,false,false,false,true,true,true,false,true,true,true,true)), None))
      ),
      toBits
    )

    lazy val shiftLeft = newFeature(
      { (x: (BigInt, Int)) => if(x._2 < 0 || x._2 >= 256) throw new IllegalArgumentException() else (x._1.asInstanceOf[BigInt].shiftLeft(x._2)) },
      "{ (x: (BigInt, Int)) => x._1.shiftLeft(x._2) }",
      FuncValue(
        Array((1, SPair(SBigInt, SInt))),
        MethodCall.typed[Value[SBigInt.type]](
          SelectField.typed[Value[SBigInt.type]](ValUse(1, SPair(SBigInt, SInt)), 1.toByte),
          SBigIntMethods.v6Methods.find(_.name == "shiftLeft").get,
          Vector(SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SBigInt, SInt)), 2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (CBigInt(BigInteger.valueOf(3)), 3) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(24))), None)),
        (CBigInt(BigInteger.valueOf(3)), 8) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(768))), None)),
        (CBigInt(BigInteger.valueOf(-2)), 10) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(-2048))), None)),
        (CBigInt(BigInteger.valueOf(-222)), 10) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(-227328L))), None)),
        (CBigInt(BigInteger.valueOf(-222)), -1) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None)),
        (CBigInt(BigInteger.valueOf(-222)), 256) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None))
      ),
      shiftLeft,
      preGeneratedSamples = Some(Seq())
    )

    lazy val shiftRight = newFeature(
      { (x: (BigInt, Int)) => if(x._2 < 0 || x._2 >= 256) throw new IllegalArgumentException() else (x._1.asInstanceOf[BigInt].shiftRight(x._2)) },
      "{ (x: (BigInt, Int)) => x._1.shiftRight(x._2) }",
      FuncValue(
        Array((1, SPair(SBigInt, SInt))),
        MethodCall.typed[Value[SBigInt.type]](
          SelectField.typed[Value[SBigInt.type]](ValUse(1, SPair(SBigInt, SInt)), 1.toByte),
          SBigIntMethods.v6Methods.find(_.name == "shiftRight").get,
          Vector(SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SBigInt, SInt)), 2.toByte)),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)

    verifyCases(
      Seq(
        (CBigInt(BigInteger.valueOf(24)), 3) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(3))), None)),
        (CBigInt(BigInteger.valueOf(1600)), 8) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(6))), None)),
        (CBigInt(BigInteger.valueOf(-3200)), 8) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(-13))), None)),
        (CBigInt(BigInteger.valueOf(-320019)), 18) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(-2))), None)),
        (CBigInt(BigInteger.valueOf(-320019)), 63) -> new Expected(ExpectedResult(Success(CBigInt(BigInteger.valueOf(-1))), None)),
        (CBigInt(BigInteger.valueOf(24)), -1) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None)),
        (CBigInt(BigInteger.valueOf(24)), 256) -> new Expected(ExpectedResult(Failure(new IllegalArgumentException()), None))
      ),
      shiftRight,
      preGeneratedSamples = Some(Seq())
    )
  }

  property("Box properties equivalence (new features)") {
    // related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/416
    def getReg = newFeature((x: Box) => x.getReg[Long](0).get,
      "{ (x: Box) => x.getReg[Long](0).get }",
      FuncValue(
        Array((1, SBox)),
        OptionGet(ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R0, SOption(SLong)))
      ),
      sinceVersion = V6SoftForkVersion)

    if (activatedVersionInTests < V6SoftForkVersion) {
      // NOTE, for such versions getReg is not supported
      // which is checked below

      forAll { box: Box =>
        Seq(getReg).foreach(_.checkEquality(box))
      }
    } else {
      val value = 10L
      val box = CBox(new ErgoBox(value, TrueTree, Colls.emptyColl[Token], Map.empty,
                                  ModifierId @@ Base16.encode(Array.fill(32)(0)), 0, 0))
      verifyCases(
        Seq(
          box -> new Expected(ExpectedResult(Success(value), None))
        ),
        getReg
      )
    }
  }

  // TODO v6.0 (3h): https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
  property("Coll find method equivalence") {
    val find = newFeature((x: Coll[Int]) => x.find({ (v: Int) => v > 0 }),
      "{ (x: Coll[Int]) => x.find({ (v: Int) => v > 0} ) }",
      sinceVersion = V6SoftForkVersion)

    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions getReg is not supported
      // which is checked below

      forAll { x: Coll[Int] =>
        find.checkEquality(x)
      }
    }
  }

  // TODO v6.0 (3h): https://github.com/ScorexFoundation/sigmastate-interpreter/issues/418
  property("Coll bitwise methods equivalence") {
    val shiftRight = newFeature(
      { (x: Coll[Boolean]) =>
        if (x.size > 2) x.slice(0, x.size - 2) else Colls.emptyColl[Boolean]
      },
      "{ (x: Coll[Boolean]) => x >> 2 }",
      sinceVersion = V6SoftForkVersion)

    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions getReg is not supported
      // which is checked below

      forAll { x: Array[Boolean] =>
        shiftRight.checkEquality(Colls.fromArray(x))
      }
    }
  }

  // TODO v6.0 (3h): https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
  property("Coll diff methods equivalence") {
    val diff = newFeature((x: (Coll[Int], Coll[Int])) => x._1.diff(x._2),
      "{ (x: (Coll[Int], Coll[Int])) => x._1.diff(x._2) }",
      sinceVersion = V6SoftForkVersion)

    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions getReg is not supported
      // which is checked below

      forAll { (x: Coll[Int], y: Coll[Int]) =>
        diff.checkEquality((x, y))
      }
    }
  }

  // TODO v6.0: implement Option.fold (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479)
  property("Option new methods") {
    val n = ExactNumeric.LongIsExactNumeric
    val fold = newFeature({ (x: Option[Long]) => x.fold(5.toLong)( (v: Long) => n.plus(v, 1) ) },
      "{ (x: Option[Long]) => x.fold(5, { (v: Long) => v + 1 }) }",
      sinceVersion = V6SoftForkVersion)

    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions getReg is not supported
      // which is checked below

      forAll { x: Option[Long] =>
        Seq(fold).map(_.checkEquality(x))
      }
    }
  }

  // TODO v6.0 (3h): implement allZK func https://github.com/ScorexFoundation/sigmastate-interpreter/issues/543
  property("allZK equivalence") {
    lazy val allZK = newFeature((x: Coll[SigmaProp]) => SigmaDsl.allZK(x),
      "{ (x: Coll[SigmaProp]) => allZK(x) }",
      sinceVersion = V6SoftForkVersion)

    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions getReg is not supported
      // which is checked below

      forAll { x: Coll[SigmaProp] =>
        allZK.checkEquality(x)
      }
    }
  }

  // TODO v6.0 (3h): implement anyZK func https://github.com/ScorexFoundation/sigmastate-interpreter/issues/543
  property("anyZK equivalence") {
    lazy val anyZK = newFeature((x: Coll[SigmaProp]) => SigmaDsl.anyZK(x),
      "{ (x: Coll[SigmaProp]) => anyZK(x) }",
      sinceVersion = V6SoftForkVersion)

    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions getReg is not supported
      // which is checked below

      forAll { x: Coll[SigmaProp] =>
        anyZK.checkEquality(x)
      }
    }
  }

  property("Numeric.toBytes methods equivalence") {
    lazy val toBytes = newFeature(
      { (x: Byte) => x.toBigEndianBytes },
      "{ (x: Byte) => x.toBytes }",
      FuncValue(
        Array((1, SByte)),
        MethodCall.typed[Value[SCollection[SByte.type]]](
          ValUse(1, SByte),
          SByteMethods.getMethodByName("toBytes"),
          Vector(),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)
    val cases = Seq(
      (0.toByte, Success(Coll(0.toByte))),
      (1.toByte, Success(Coll(1.toByte)))
    )

    testCases(cases, toBytes)
  }

  property("Fix substConstants in v6.0 for ErgoTree version > 0") {
    // tree with one segregated constant and v0
    val t1 = ErgoTree(
      header = ErgoTree.setConstantSegregation(ZeroHeader),
      constants = Vector(TrueSigmaProp),
      ConstantPlaceholder(0, SSigmaProp))

    // tree with one segregated constant and max supported version
    val t2 = ErgoTree(
      header = ErgoTree.setConstantSegregation(
        ErgoTree.headerWithVersion(ZeroHeader, VersionContext.MaxSupportedScriptVersion)
      ),
      Vector(TrueSigmaProp),
      ConstantPlaceholder(0, SSigmaProp))

    def costDetails(nItems: Int) = TracedCost(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(ConcreteCollection),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(ConcreteCollection),
        FixedCostItem(Constant),
        FixedCostItem(BoolToSigmaProp),
        ast.SeqCostItem(CompanionDesc(SubstConstants), PerItemCost(JitCost(100), JitCost(100), 1), nItems)
      )
    )
    val expectedTreeBytes_beforeV6 = Helpers.decodeBytes("1b0108d27300")
    val expectedTreeBytes_V6 = Helpers.decodeBytes("1b050108d27300")

    verifyCases(
      Seq(
        // for tree v0, the result is the same for all versions
        (Coll(t1.bytes: _*), 0) -> Expected(
          Success(Helpers.decodeBytes("100108d27300")),
          cost = 1793,
          expectedDetails = CostDetails.ZeroCost,
          newCost = 2065,
          newVersionedResults = expectedSuccessForAllTreeVersions(Helpers.decodeBytes("100108d27300"), 2065, costDetails(1))
        ),
        // for tree version > 0, the result depend on activated version
        (Coll(t2.bytes: _*), 0) -> Expected(
          Success(expectedTreeBytes_beforeV6),
          cost = 1793,
          expectedDetails = CostDetails.ZeroCost,
          newCost = 2065,
          newVersionedResults = expectedSuccessForAllTreeVersions(expectedTreeBytes_V6, 2065, costDetails(1)))
      ),
      changedFeature(
        changedInVersion = VersionContext.V6SoftForkVersion,
        { (x: (Coll[Byte], Int)) =>
          SigmaDsl.substConstants(x._1, Coll[Int](x._2), Coll[Any](SigmaDsl.sigmaProp(false))(sigma.AnyType))
        },
        { (x: (Coll[Byte], Int)) =>
          SigmaDsl.substConstants(x._1, Coll[Int](x._2), Coll[Any](SigmaDsl.sigmaProp(false))(sigma.AnyType))
        },
        "{ (x: (Coll[Byte], Int)) => substConstants[Any](x._1, Coll[Int](x._2), Coll[Any](sigmaProp(false))) }",
        FuncValue(
          Vector((1, SPair(SByteArray, SInt))),
          SubstConstants(
            SelectField.typed[Value[SCollection[SByte.type]]](ValUse(1, SPair(SByteArray, SInt)), 1.toByte),
            ConcreteCollection(
              Array(SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SByteArray, SInt)), 2.toByte)),
              SInt
            ),
            ConcreteCollection(Array(BoolToSigmaProp(FalseLeaf)), SSigmaProp)
          )
        )
      )
    )

    // before v6.0 the expected tree is not parsable
    ErgoTree.fromBytes(expectedTreeBytes_beforeV6.toArray).isRightParsed shouldBe false

    // in v6.0 the expected tree should be parsable and similar to the original tree
    val tree = ErgoTree.fromBytes(expectedTreeBytes_V6.toArray)
    tree.isRightParsed shouldBe true
    tree.header shouldBe t2.header
    tree.constants.length shouldBe t2.constants.length
    tree.root shouldBe t2.root
  }

  property("Header new methods") {

    def checkPoW = {
      newFeature(
        { (x: Header) => x.checkPow},
        "{ (x: Header) => x.checkPow }",
        FuncValue(
          Array((1, SHeader)),
          MethodCall.typed[Value[SBoolean.type]](
            ValUse(1, SHeader),
            SHeaderMethods.checkPowMethod,
            IndexedSeq(),
            Map()
          )
        ),
        sinceVersion = VersionContext.V6SoftForkVersion
      )
    }

    // bytes of real mainnet block header at height 614,440
    val headerBytes = "02ac2101807f0000ca01ff0119db227f202201007f62000177a080005d440896d05d3f80dcff7f5e7f59007294c180808d0158d1ff6ba10000f901c7f0ef87dcfff17fffacb6ff7f7f1180d2ff7f1e24ffffe1ff937f807f0797b9ff6ebdae007e5c8c00b8403d3701557181c8df800001b6d5009e2201c6ff807d71808c00019780f087adb3fcdbc0b3441480887f80007f4b01cf7f013ff1ffff564a0000b9a54f00770e807f41ff88c00240000080c0250000000003bedaee069ff4829500b3c07c4d5fe6b3ea3d3bf76c5c28c1d4dcdb1bed0ade0c0000000000003105"
    val header1 = new CHeader(ErgoHeader.sigmaSerializer.fromBytes(Base16.decode(headerBytes).get))

    verifyCases(
      Seq(
        header1 -> new Expected(ExpectedResult(Success(true), None))
      ),
      checkPoW
    )
  }

  property("higher order lambdas") {
    val f = newFeature[Coll[Int], Coll[Int]](
      { (xs: Coll[Int]) =>
        val inc = { (x: Int) => x + 1 }

        def apply(in: (Int => Int, Int)) = in._1(in._2)

        xs.map { (x: Int) => apply((inc, x)) }
      },
      """{(xs: Coll[Int]) =>
        |   val inc = { (x: Int) => x + 1 }
        |   def apply(in: (Int => Int, Int)) = in._1(in._2)
        |   xs.map { (x: Int) => apply((inc, x)) }
        | }
        |""".stripMargin,
      FuncValue(
        Array((1, SCollectionType(SInt))),
        MapCollection(
          ValUse(1, SCollectionType(SInt)),
          FuncValue(
            Array((3, SInt)),
            Apply(
              FuncValue(
                Array((5, SPair(SFunc(Array(SInt), SInt, List()), SInt))),
                Apply(
                  SelectField.typed[Value[SFunc]](
                    ValUse(5, SPair(SFunc(Array(SInt), SInt, List()), SInt)),
                    1.toByte
                  ),
                  Array(
                    SelectField.typed[Value[SInt.type]](
                      ValUse(5, SPair(SFunc(Array(SInt), SInt, List()), SInt)),
                      2.toByte
                    )
                  )
                )
              ),
              Array(
                Tuple(
                  Vector(
                    FuncValue(
                      Array((5, SInt)),
                      ArithOp(ValUse(5, SInt), IntConstant(1), OpCode @@ (-102.toByte))
                    ),
                    ValUse(3, SInt)
                  )
                )
              )
            )
          )
        )
      ),
    sinceVersion = VersionContext.V6SoftForkVersion
    )

    verifyCases(
      Seq(
        Coll(1, 2) -> Expected(
          Success(Coll(2, 3)),
          cost = 1793,
          expectedDetails = CostDetails.ZeroCost
        )
      ),
      f,
      preGeneratedSamples = Some(Seq(
        Coll(Int.MinValue, Int.MaxValue - 1),
        Coll(0, 1, 2, 3, 100, 1000)
      ))
    )
  }

  property("Global.deserializeTo - group element") {
    def deserializeTo: Feature[GroupElement, Boolean] = {
      newFeature(
        { (x: GroupElement) => CSigmaDslBuilder.deserializeTo[GroupElement](x.getEncoded) == x},
        "{ (x: GroupElement) => Global.deserializeTo[GroupElement](x.getEncoded) == x }",
        FuncValue(
          Array((1, SGroupElement)),
          EQ(
            MethodCall.typed[Value[SGroupElement.type]](
              Global,
              SGlobalMethods.deserializeToMethod.withConcreteTypes(Map(tT -> SGroupElement)),
              Vector(
                MethodCall.typed[Value[SCollection[SByte.type]]](
                  ValUse(1, SGroupElement),
                  SGroupElementMethods.getMethodByName("getEncoded"),
                  IndexedSeq(),
                  Map()
                )
              ),
              Map(STypeVar("T") -> SGroupElement)
            ),
            ValUse(1, SGroupElement)
          )
        ),
        sinceVersion = VersionContext.V6SoftForkVersion
      )
    }

    verifyCases(
      Seq(
        CGroupElement(SecP256K1Group.generator) -> new Expected(ExpectedResult(Success(true), None))
      ),
      deserializeTo
    )
  }

  property("Global.deserializeTo - header") {
    val headerBytes = "02ac2101807f0000ca01ff0119db227f202201007f62000177a080005d440896d05d3f80dcff7f5e7f59007294c180808d0158d1ff6ba10000f901c7f0ef87dcfff17fffacb6ff7f7f1180d2ff7f1e24ffffe1ff937f807f0797b9ff6ebdae007e5c8c00b8403d3701557181c8df800001b6d5009e2201c6ff807d71808c00019780f087adb3fcdbc0b3441480887f80007f4b01cf7f013ff1ffff564a0000b9a54f00770e807f41ff88c00240000080c0250000000003bedaee069ff4829500b3c07c4d5fe6b3ea3d3bf76c5c28c1d4dcdb1bed0ade0c0000000000003105"
    val header1 = new CHeader(ErgoHeader.sigmaSerializer.fromBytes(Base16.decode(headerBytes).get))

    // v1 header below
    val header2Bytes = "010000000000000000000000000000000000000000000000000000000000000000766ab7a313cd2fb66d135b0be6662aa02dfa8e5b17342c05a04396268df0bfbb93fb06aa44413ff57ac878fda9377207d5db0e78833556b331b4d9727b3153ba18b7a08878f2a7ee4389c5a1cece1e2724abe8b8adc8916240dd1bcac069177303f1f6cee9ba2d0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8060117650100000003be7ad70c74f691345cbedba19f4844e7fc514e1188a7929f5ae261d5bb00bb6602da9385ac99014ddcffe88d2ac5f28ce817cd615f270a0a5eae58acfb9fd9f6a0000000030151dc631b7207d4420062aeb54e82b0cfb160ff6ace90ab7754f942c4c3266b"
    val header2 = new CHeader(ErgoHeader.sigmaSerializer.fromBytes(Base16.decode(header2Bytes).get))

    def deserializeTo: Feature[Header, Boolean] = {
      newFeature(
        { (x: Header) => CSigmaDslBuilder.deserializeTo[Header](CSigmaDslBuilder.serialize(x)) == x},
        "{ (x: Header) => Global.deserializeTo[Header](serialize(x)) == x }",
        sinceVersion = VersionContext.V6SoftForkVersion
      )
    }

    verifyCases(
      Seq(
        header1 -> new Expected(ExpectedResult(Success(true), None)),
        header2 -> new Expected(ExpectedResult(Success(true), None))
      ),
      deserializeTo
    )
  }

  property("Global.serialize & deserialize roundtrip - BigInt") {
    import sigma.data.OrderingOps.BigIntOrdering

    def deserializeTo: Feature[BigInt, Boolean] = {
      newFeature(
        { (x: BigInt) => CSigmaDslBuilder.deserializeTo[BigInt](CSigmaDslBuilder.serialize(x)) == x},
        "{ (x: BigInt) => Global.deserializeTo[BigInt](serialize(x)) == x }",
        sinceVersion = VersionContext.V6SoftForkVersion
      )
    }

    val cases = Seq(
      (CBigInt(BigInteger.ONE), new Expected(ExpectedResult(Success(true), None))),
      (CBigInt(sigma.crypto.SecP256K1Group.q.divide(new BigInteger("2"))), new Expected(ExpectedResult(Success(true), None))),
      (CBigInt(sigma.crypto.SecP256K1Group.p.divide(new BigInteger("2"))), new Expected(ExpectedResult(Success(true), None)))
    )
    verifyCases(cases, deserializeTo)
  }

}
