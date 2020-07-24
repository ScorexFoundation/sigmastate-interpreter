package special.sigma

import java.math.BigInteger

import org.ergoplatform.ErgoScriptPredef.TrueProp
import org.ergoplatform.dsl.{SigmaContractSyntax, TestContractSpec}
import org.ergoplatform._
import org.ergoplatform.settings.ErgoAlgos
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.{PropertyChecks, TableFor2}
import org.scalatest.{PropSpec, Matchers, Tag}
import scalan.{ExactNumeric, RType}
import org.scalactic.source
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADDigest, ADKey, ADValue}
import scorex.crypto.hash.{Digest32, Blake2b256}
import scalan.util.Extensions._
import sigma.util.Extensions._
import sigmastate.SCollection._
import sigmastate.Values.IntConstant
import sigmastate._
import sigmastate.basics.DLogProtocol._
import sigmastate.Values._
import sigmastate.lang.Terms.Apply
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.helpers.SigmaPPrint
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.Terms.MethodCall
import sigmastate.utxo._
import special.collection.Coll
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utils.Helpers

import scala.reflect.ClassTag
import scala.util.{DynamicVariable, Success, Failure, Try}
import OrderingOps._
import scorex.util.ModifierId

/** This suite tests every method of every SigmaDsl type to be equivalent to
  * the evaluation of the corresponding ErgoScript operation */
class SigmaDslSpec extends SigmaDslTesting { suite =>

  override implicit val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 30)

  val PrintTestCasesDefault: Boolean = false
  val FailOnTestVectorsDefault: Boolean = true

  def testCases[A: Ordering: Arbitrary: ClassTag, B](cases: Seq[(A, Try[B])],
                                f: FeatureTest[A, B],
                                printTestCases: Boolean = PrintTestCasesDefault,
                                failOnTestVectors: Boolean = FailOnTestVectorsDefault): Unit = {
    val table = Table(("x", "y"), cases:_*)
    forAll(table) { (x: A, expectedRes: Try[B]) =>
      val res = f.checkEquality(x, printTestCases)

      // TODO HF: remove this `if` once newImpl is implemented
      if (f.featureType == ExistingFeature) {
        (res, expectedRes) match {
          case (Failure(exception), Failure(expectedException)) =>
            exception.getClass shouldBe expectedException.getClass
            exception.getMessage shouldBe expectedException.getMessage
          case _ =>
            if (failOnTestVectors) {
              assertResult(expectedRes, s"Actual: ${SigmaPPrint(res, height = 150).plainText}")(res)
            }
            else {
              if (expectedRes != res) {
                print("Actual: ")
                SigmaPPrint.pprintln(res, height = 150)
              }
            }
        }
      }
    }
    test(f, printTestCases)
  }

  def test[A: Arbitrary: Ordering: ClassTag, B](f: FeatureTest[A,B], printTestCases: Boolean = PrintTestCasesDefault): Unit = {
    // first generate all test inputs
    val inputs = scala.collection.mutable.ArrayBuilder.make[A]()
    forAll { (x: A) =>
      inputs += x
    }
    // then tests them in the sorted order, this will output a nice log of test cases
    inputs.result().sorted.foreach { x =>
      f.checkEquality(x, printTestCases)
    }
  }

  ///=====================================================
  ///              Boolean type operations
  ///-----------------------------------------------------

  property("Boolean methods equivalence") {
    val toByte = newFeature((x: Boolean) => x.toByte, "{ (x: Boolean) => x.toByte }")

    val cases = Seq(
      (true, Success(1.toByte)),
      (false, Success(0.toByte))
    )

    testCases(cases, toByte)
  }

  property("BinXor(logical XOR) equivalence") {
    val binXor = existingFeature((x: (Boolean, Boolean)) => x._1 ^ x._2,
      "{ (x: (Boolean, Boolean)) => x._1 ^ x._2 }",
      FuncValue(
        Vector((1, STuple(Vector(SBoolean, SBoolean)))),
        BinXor(
          SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SBoolean, SBoolean))), 1.toByte),
          SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SBoolean, SBoolean))), 2.toByte)
        )
      ))
    val cases = Seq(
      ((true, true), Try(false)),
      ((true, false), Try(true)),
      ((false, false), Try(false)),
      ((false, true), Try(true))
    )
    testCases(cases, binXor)
  }

  property("BinXor(logical XOR) test") {
    val xor = existingFeature((x: (Int, Boolean)) => (x._1 == 0) ^ x._2,
      "{ (x: (Int, Boolean)) => (x._1 == 0) ^ x._2 }",
      FuncValue(
        Vector((1, STuple(Vector(SInt, SBoolean)))),
        BinXor(
          EQ(
            SelectField.typed[IntValue](ValUse(1, STuple(Vector(SInt, SBoolean))), 1.toByte),
            IntConstant(0)
          ),
          SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SInt, SBoolean))), 2.toByte)
        )
      ))
    val cases = Seq(
      ((1095564593, true), Success(true)),
      ((-901834021, true), Success(true)),
      ((595045530, false), Success(false)),
      ((-1157998227, false), Success(false)),
      ((0, true), Success(false)),
      ((0, false), Success(true))
    )
    testCases(cases, xor)
  }

  property("&& boolean equivalence") {
    lazy val eq = existingFeature((x:(Boolean, Boolean)) => x._1 && x._2,
      "{ (x:(Boolean, Boolean)) => x._1 && x._2 }",
      FuncValue(
        Vector((1, STuple(Vector(SBoolean, SBoolean)))),
        BinAnd(
          SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SBoolean, SBoolean))), 1.toByte),
          SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SBoolean, SBoolean))), 2.toByte)
        )
      ))
    val cases = Seq(
      ((false, true), Success(false)),
      ((false, false), Success(false)),
      ((true, true), Success(true)),
      ((true, false), Success(false))
    )
    testCases(cases, eq)
  }

  property("|| boolean equivalence") {
    lazy val eq = existingFeature((x:(Boolean, Boolean)) => x._1 || x._2,
      "{ (x:(Boolean, Boolean)) => x._1 || x._2 }",
      FuncValue(
        Vector((1, STuple(Vector(SBoolean, SBoolean)))),
        BinOr(
          SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SBoolean, SBoolean))), 1.toByte),
          SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SBoolean, SBoolean))), 2.toByte)
        )
      ))
    val cases = Seq(
      ((true, false), Success(true)),
      ((true, true), Success(true)),
      ((false, false), Success(false)),
      ((false, true), Success(true))
    )
    testCases(cases, eq)
  }

  property("lazy || and && boolean equivalence") {
    testCases(
      Seq(
        (true, Success(true)),
        (false, Failure(new ArithmeticException("/ by zero")))
      ),
      existingFeature((x: Boolean) => x || (1 / 0 == 1),
        "{ (x: Boolean) => x || (1 / 0 == 1) }",
        FuncValue(
          Vector((1, SBoolean)),
          BinOr(
            ValUse(1, SBoolean),
            EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
          )
        )))

    testCases(
      Seq(
        (true, Failure(new ArithmeticException("/ by zero"))),
        (false, Success(false))
      ),
      existingFeature((x: Boolean) => x && (1 / 0 == 1),
        "{ (x: Boolean) => x && (1 / 0 == 1) }",
        FuncValue(
          Vector((1, SBoolean)),
          BinAnd(
            ValUse(1, SBoolean),
            EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
          )
        )))

    testCases(
      Seq(
        (false, Success(false)),
        (true, Success(true))
      ),
      existingFeature((x: Boolean) => x && (x || (1 / 0 == 1)),
        "{ (x: Boolean) => x && (x || (1 / 0 == 1)) }",
        FuncValue(
          Vector((1, SBoolean)),
          BinAnd(
            ValUse(1, SBoolean),
            BinOr(
              ValUse(1, SBoolean),
              EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
            )
          )
        )))

    testCases(
      Seq(
        (false, Success(false)),
        (true, Success(true))
      ),
      existingFeature((x: Boolean) => x && (x && (x || (1 / 0 == 1))),
        "{ (x: Boolean) => x && (x && (x || (1 / 0 == 1))) }",
        FuncValue(
          Vector((1, SBoolean)),
          BinAnd(
            ValUse(1, SBoolean),
            BinAnd(
              ValUse(1, SBoolean),
              BinOr(
                ValUse(1, SBoolean),
                EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
              )
            )
          )
        )))

    testCases(
      Seq(
        (false, Success(false)),
        (true, Success(true))
      ),
      existingFeature((x: Boolean) => x && (x && (x && (x || (1 / 0 == 1)))),
        "{ (x: Boolean) => x && (x && (x && (x || (1 / 0 == 1)))) }",
        FuncValue(
          Vector((1, SBoolean)),
          BinAnd(
            ValUse(1, SBoolean),
            BinAnd(
              ValUse(1, SBoolean),
              BinAnd(
                ValUse(1, SBoolean),
                BinOr(
                  ValUse(1, SBoolean),
                  EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
                )
              )
            )
          )
        )))

    testCases(
      Seq(
        (false, Failure(new ArithmeticException("/ by zero"))),
        (true, Success(true))
      ),
      existingFeature((x: Boolean) => !(!x && (1 / 0 == 1)) && (x || (1 / 0 == 1)),
        "{ (x: Boolean) => !(!x && (1 / 0 == 1)) && (x || (1 / 0 == 1)) }",
        FuncValue(
          Vector((1, SBoolean)),
          BinAnd(
            LogicalNot(
              BinAnd(
                LogicalNot(ValUse(1, SBoolean)),
                EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
              )
            ),
            BinOr(
              ValUse(1, SBoolean),
              EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
            )
          )
        )))

    testCases(
      Seq(
        (true, Success(true)),
        (false, Failure(new ArithmeticException("/ by zero")))
      ),
      existingFeature((x: Boolean) => (x || (1 / 0 == 1)) && x,
        "{ (x: Boolean) => (x || (1 / 0 == 1)) && x }",
        FuncValue(
          Vector((1, SBoolean)),
          BinAnd(
            BinOr(
              ValUse(1, SBoolean),
              EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
            ),
            ValUse(1, SBoolean)
          )
        )))

    testCases(
      Seq(
        (true, Success(true)),
        (false, Failure(new ArithmeticException("/ by zero")))
      ),
      existingFeature((x: Boolean) => (x || (1 / 0 == 1)) && (x || (1 / 0 == 1)),
        "{ (x: Boolean) => (x || (1 / 0 == 1)) && (x || (1 / 0 == 1)) }",
        FuncValue(
          Vector((1, SBoolean)),
          BinAnd(
            BinOr(
              ValUse(1, SBoolean),
              EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
            ),
            BinOr(
              ValUse(1, SBoolean),
              EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
            )
          )
        )))

    testCases(
      Seq(
        (true, Success(true)),
        (false, Failure(new ArithmeticException("/ by zero")))
      ),
      existingFeature(
        (x: Boolean) => (!(!x && (1 / 0 == 1)) || (1 / 0 == 0)) && (x || (1 / 0 == 1)),
        "{ (x: Boolean) => (!(!x && (1 / 0 == 1)) || (1 / 0 == 0)) && (x || (1 / 0 == 1)) }",
        FuncValue(
          Vector((1, SBoolean)),
          BinAnd(
            BinOr(
              LogicalNot(
                BinAnd(
                  LogicalNot(ValUse(1, SBoolean)),
                  EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
                )
              ),
              EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(0))
            ),
            BinOr(
              ValUse(1, SBoolean),
              EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
            )
          )
        )))

    testCases(
      Seq(
        (false, Failure(new ArithmeticException("/ by zero"))),
        (true, Success(true))
      ),
      existingFeature(
        (x: Boolean) => (!(!x && (1 / 0 == 1)) || (1 / 0 == 0)) && (!(!x && (1 / 0 == 1)) || (1 / 0 == 1)),
        "{ (x: Boolean) => (!(!x && (1 / 0 == 1)) || (1 / 0 == 0)) && (!(!x && (1 / 0 == 1)) || (1 / 0 == 1)) }",
        FuncValue(
          Vector((1, SBoolean)),
          BlockValue(
            Vector(ValDef(3, List(), LogicalNot(ValUse(1, SBoolean)))),
            BinAnd(
              BinOr(
                LogicalNot(
                  BinAnd(
                    ValUse(3, SBoolean),
                    EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
                  )
                ),
                EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(0))
              ),
              BinOr(
                LogicalNot(
                  BinAnd(
                    ValUse(3, SBoolean),
                    EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
                  )
                ),
                EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
              )
            )
          )
        )))
  }

  property("Byte methods equivalence") {
    testCases(
      Seq(
        (0.toByte, Success(0.toByte)),
        (1.toByte, Success(1.toByte)),
        (55.toByte, Success(55.toByte)),
        (Byte.MaxValue, Success(Byte.MaxValue)),
        (-1.toByte, Success(-1.toByte)),
        (-65.toByte, Success(-65.toByte)),
        (Byte.MinValue, Success(Byte.MinValue))
      ),
      existingFeature(
        (x: Byte) => x.toByte, "{ (x: Byte) => x.toByte }",
        FuncValue(Vector((1, SByte)), ValUse(1, SByte))))

    testCases(
      Seq(
        (0.toByte, Success(0.toShort)),
        (1.toByte, Success(1.toShort)),
        (55.toByte, Success(55.toShort)),
        (Byte.MaxValue, Success(Byte.MaxValue.toShort)),
        (-1.toByte, Success(-1.toShort)),
        (-65.toByte, Success(-65.toShort)),
        (Byte.MinValue, Success(Byte.MinValue.toShort))
      ),
      existingFeature(
        (x: Byte) => x.toShort, "{ (x: Byte) => x.toShort }",
        FuncValue(Vector((1, SByte)), Upcast(ValUse(1, SByte), SShort))))

    testCases(
      Seq(
        (0.toByte, Success(0)),
        (1.toByte, Success(1)),
        (55.toByte, Success(55)),
        (Byte.MaxValue, Success(Byte.MaxValue.toInt)),
        (-1.toByte, Success(-1)),
        (-65.toByte, Success(-65)),
        (Byte.MinValue, Success(Byte.MinValue.toInt))
      ),
      existingFeature(
        (x: Byte) => x.toInt, "{ (x: Byte) => x.toInt }",
        FuncValue(Vector((1, SByte)), Upcast(ValUse(1, SByte), SInt))))

    testCases(
      Seq(
        (0.toByte, Success(0L)),
        (1.toByte, Success(1L)),
        (55.toByte, Success(55L)),
        (Byte.MaxValue, Success(Byte.MaxValue.toLong)),
        (-1.toByte, Success(-1L)),
        (-65.toByte, Success(-65L)),
        (Byte.MinValue, Success(Byte.MinValue.toLong))
      ),
      existingFeature(
        (x: Byte) => x.toLong, "{ (x: Byte) => x.toLong }",
        FuncValue(Vector((1, SByte)), Upcast(ValUse(1, SByte), SLong))))

    testCases(
      Seq(
        (0.toByte, Success(CBigInt(new BigInteger("0", 16)))),
        (1.toByte, Success(CBigInt(new BigInteger("1", 16)))),
        (-1.toByte, Success(CBigInt(new BigInteger("-1", 16)))),
        (127.toByte, Success(CBigInt(new BigInteger("7f", 16)))),
        (-128.toByte, Success(CBigInt(new BigInteger("-80", 16)))),
        (90.toByte, Success(CBigInt(new BigInteger("5a", 16)))),
        (-53.toByte, Success(CBigInt(new BigInteger("-35", 16))))
      ),
      existingFeature(
        (x: Byte) => x.toBigInt, "{ (x: Byte) => x.toBigInt }",
        FuncValue(Vector((1, SByte)), Upcast(ValUse(1, SByte), SBigInt))))

    val n = ExactNumeric.ByteIsExactNumeric
    testCases(
      Seq(
        ((-128.toByte, -128.toByte), Failure(new ArithmeticException("Byte overflow"))),
        ((-128.toByte, 0.toByte), Failure(new ArithmeticException("/ by zero"))),
        ((-128.toByte, 17.toByte), Failure(new ArithmeticException("Byte overflow"))),
        ((-128.toByte, 127.toByte), Failure(new ArithmeticException("Byte overflow"))),
        ((-120.toByte, 82.toByte), Failure(new ArithmeticException("Byte overflow"))),
        ((-103.toByte, 1.toByte), Success((-102.toByte, (-104.toByte, (-103.toByte, (-103.toByte, 0.toByte)))))),
        ((-90.toByte, 37.toByte), Failure(new ArithmeticException("Byte overflow"))),
        ((-78.toByte, -111.toByte), Failure(new ArithmeticException("Byte overflow"))),
        ((-71.toByte, -44.toByte), Failure(new ArithmeticException("Byte overflow"))),
        ((-53.toByte, 0.toByte), Failure(new ArithmeticException("/ by zero"))),
        ((-34.toByte, 8.toByte), Failure(new ArithmeticException("Byte overflow"))),
        ((-24.toByte, 127.toByte), Failure(new ArithmeticException("Byte overflow"))),
        ((-1.toByte, -1.toByte), Success((-2.toByte, (0.toByte, (1.toByte, (1.toByte, 0.toByte)))))),
        ((-1.toByte, 23.toByte), Success((22.toByte, (-24.toByte, (-23.toByte, (0.toByte, -1.toByte)))))),
        ((0.toByte, -128.toByte), Failure(new ArithmeticException("Byte overflow"))),
        ((0.toByte, -23.toByte), Success((-23.toByte, (23.toByte, (0.toByte, (0.toByte, 0.toByte)))))),
        ((0.toByte, -1.toByte), Success((-1.toByte, (1.toByte, (0.toByte, (0.toByte, 0.toByte)))))),
        ((0.toByte, 0.toByte), Failure(new ArithmeticException("/ by zero"))),
        ((0.toByte, 1.toByte), Success((1.toByte, (-1.toByte, (0.toByte, (0.toByte, 0.toByte)))))),
        ((0.toByte, 60.toByte), Success((60.toByte, (-60.toByte, (0.toByte, (0.toByte, 0.toByte)))))),
        ((0.toByte, 127.toByte), Success((127.toByte, (-127.toByte, (0.toByte, (0.toByte, 0.toByte)))))),
        ((1.toByte, -1.toByte), Success((0.toByte, (2.toByte, (-1.toByte, (-1.toByte, 0.toByte)))))),
        ((1.toByte, 0.toByte), Failure(new ArithmeticException("/ by zero"))),
        ((1.toByte, 26.toByte), Success((27.toByte, (-25.toByte, (26.toByte, (0.toByte, 1.toByte)))))),
        ((7.toByte, -32.toByte), Failure(new ArithmeticException("Byte overflow"))),
        ((33.toByte, 1.toByte), Success((34.toByte, (32.toByte, (33.toByte, (33.toByte, 0.toByte)))))),
        ((90.toByte, 0.toByte), Failure(new ArithmeticException("/ by zero"))),
        ((127.toByte, -128.toByte), Failure(new ArithmeticException("Byte overflow"))),
        ((127.toByte, -47.toByte), Failure(new ArithmeticException("Byte overflow"))),
        ((127.toByte, 127.toByte), Failure(new ArithmeticException("Byte overflow")))
      ),
      existingFeature(
        { (x: (Byte, Byte)) =>
          val a = x._1; val b = x._2
          val plus = n.plus(a, b)
          val minus = n.minus(a, b)
          val mul = n.times(a, b)
          val div = (a / b).toByteExact
          val mod = (a % b).toByteExact
          (plus, (minus, (mul, (div, mod))))
        },
        """{ (x: (Byte, Byte)) =>
         |  val a = x._1; val b = x._2
         |  val plus = a + b
         |  val minus = a - b
         |  val mul = a * b
         |  val div = a / b
         |  val mod = a % b
         |  (plus, (minus, (mul, (div, mod))))
         |}""".stripMargin,
        FuncValue(
          Vector((1, STuple(Vector(SByte, SByte)))),
          BlockValue(
            Vector(
              ValDef(
                3,
                List(),
                SelectField.typed[ByteValue](ValUse(1, STuple(Vector(SByte, SByte))), 1.toByte)
              ),
              ValDef(
                4,
                List(),
                SelectField.typed[ByteValue](ValUse(1, STuple(Vector(SByte, SByte))), 2.toByte)
              )
            ),
            Tuple(
              Vector(
                ArithOp(ValUse(3, SByte), ValUse(4, SByte), OpCode @@ (-102.toByte)),
                Tuple(
                  Vector(
                    ArithOp(ValUse(3, SByte), ValUse(4, SByte), OpCode @@ (-103.toByte)),
                    Tuple(
                      Vector(
                        ArithOp(ValUse(3, SByte), ValUse(4, SByte), OpCode @@ (-100.toByte)),
                        Tuple(
                          Vector(
                            ArithOp(ValUse(3, SByte), ValUse(4, SByte), OpCode @@ (-99.toByte)),
                            ArithOp(ValUse(3, SByte), ValUse(4, SByte), OpCode @@ (-98.toByte))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ))
  }

  property("Byte methods equivalence (new features)") {
    lazy val toBytes = newFeature((x: Byte) => x.toBytes, "{ (x: Byte) => x.toBytes }")
    lazy val toBits = newFeature((x: Byte) => x.toBits, "{ (x: Byte) => x.toBits }")
    lazy val toAbs = newFeature((x: Byte) => x.toAbs, "{ (x: Byte) => x.toAbs }")
    lazy val compareTo = newFeature(
      (x: (Byte, Byte)) => x._1.compareTo(x._2),
      "{ (x: (Byte, Byte)) => x._1.compareTo(x._2) }")

    lazy val bitOr = newFeature(
    { (x: (Byte, Byte)) => (x._1 | x._2).toByteExact },
    "{ (x: (Byte, Byte)) => (x._1 | x._2).toByteExact }")

    lazy val bitAnd = newFeature(
    { (x: (Byte, Byte)) => (x._1 & x._2).toByteExact },
    "{ (x: (Byte, Byte)) => (x._1 & x._2).toByteExact }")

    forAll { x: Byte =>
      Seq(toBytes, toBits, toAbs).foreach(f => f.checkEquality(x))
    }

    forAll { x: (Byte, Byte) =>
      Seq(compareTo, bitOr, bitAnd).foreach(_.checkEquality(x))
    }
  }

  property("Short methods equivalence") {
    testCases(
      Seq(
        (Short.MinValue, Failure(new ArithmeticException("Byte overflow"))),
        (-21626.toShort, Failure(new ArithmeticException("Byte overflow"))),
        (Byte.MinValue.toShort, Success(Byte.MinValue)),
        (-1.toShort, Success(-1.toByte)),
        (0.toShort, Success(0.toByte)),
        (1.toShort, Success(1.toByte)),
        (Byte.MaxValue.toShort, Success(Byte.MaxValue)),
        (11768.toShort, Failure(new ArithmeticException("Byte overflow"))),
        (Short.MaxValue, Failure(new ArithmeticException("Byte overflow")))
      ),
      existingFeature((x: Short) => x.toByteExact,
        "{ (x: Short) => x.toByte }",
        FuncValue(Vector((1, SShort)), Downcast(ValUse(1, SShort), SByte))))

    testCases(
      Seq(
        (-32768.toShort, Success(-32768.toShort)),
        (-27798.toShort, Success(-27798.toShort)),
        (-1.toShort, Success(-1.toShort)),
        (0.toShort, Success(0.toShort)),
        (1.toShort, Success(1.toShort)),
        (27929.toShort, Success(27929.toShort)),
        (32767.toShort, Success(32767.toShort))
      ),
      existingFeature((x: Short) => x.toShort,
        "{ (x: Short) => x.toShort }",
        FuncValue(Vector((1, SShort)), ValUse(1, SShort))))

    testCases(
      Seq(
        (-32768.toShort, Success(-32768)),
        (-21064.toShort, Success(-21064)),
        (-1.toShort, Success(-1)),
        (0.toShort, Success(0)),
        (1.toShort, Success(1)),
        (18388.toShort, Success(18388)),
        (32767.toShort, Success(32767))
      ),
      existingFeature((x: Short) => x.toInt,
        "{ (x: Short) => x.toInt }",
        FuncValue(Vector((1, SShort)), Upcast(ValUse(1, SShort), SInt))))

    testCases(
      Seq(
        (-32768.toShort, Success(-32768L)),
        (-23408.toShort, Success(-23408L)),
        (-1.toShort, Success(-1L)),
        (0.toShort, Success(0L)),
        (1.toShort, Success(1L)),
        (23318.toShort, Success(23318L)),
        (32767.toShort, Success(32767L))
      ),
      existingFeature((x: Short) => x.toLong,
        "{ (x: Short) => x.toLong }",
        FuncValue(Vector((1, SShort)), Upcast(ValUse(1, SShort), SLong))))
        
    testCases(
      Seq(
        (-32768.toShort, Success(CBigInt(new BigInteger("-8000", 16)))),
        (-26248.toShort, Success(CBigInt(new BigInteger("-6688", 16)))),
        (-1.toShort, Success(CBigInt(new BigInteger("-1", 16)))),
        (0.toShort, Success(CBigInt(new BigInteger("0", 16)))),
        (1.toShort, Success(CBigInt(new BigInteger("1", 16)))),
        (22845.toShort, Success(CBigInt(new BigInteger("593d", 16)))),
        (32767.toShort, Success(CBigInt(new BigInteger("7fff", 16))))
      ),
      existingFeature((x: Short) => x.toBigInt,
        "{ (x: Short) => x.toBigInt }",
        FuncValue(Vector((1, SShort)), Upcast(ValUse(1, SShort), SBigInt))))

    val n = ExactNumeric.ShortIsExactNumeric
    testCases(
      Seq(
        ((-32768.toShort, 1.toShort), Failure(new ArithmeticException("Short overflow"))),
        ((-32768.toShort, 4006.toShort), Failure(new ArithmeticException("Short overflow"))),
        ((-21384.toShort, 0.toShort), Failure(new ArithmeticException("/ by zero"))),
        ((-19027.toShort, 6073.toShort), Failure(new ArithmeticException("Short overflow"))),
        ((-16800.toShort, 32767.toShort), Failure(new ArithmeticException("Short overflow"))),
        ((-1.toShort, -30005.toShort), Success((-30006.toShort, (30004.toShort, (30005.toShort, (0.toShort, -1.toShort)))))),
        ((-1.toShort, 0.toShort), Failure(new ArithmeticException("/ by zero"))),
        ((0.toShort, -1.toShort), Success((-1.toShort, (1.toShort, (0.toShort, (0.toShort, 0.toShort)))))),
        ((0.toShort, 0.toShort), Failure(new ArithmeticException("/ by zero"))),
        ((0.toShort, 1.toShort), Success((1.toShort, (-1.toShort, (0.toShort, (0.toShort, 0.toShort)))))),
        ((0.toShort, 25105.toShort), Success((25105.toShort, (-25105.toShort, (0.toShort, (0.toShort, 0.toShort)))))),
        ((1.toShort, -32768.toShort), Failure(new ArithmeticException("Short overflow"))),
        ((1.toShort, -1.toShort), Success((0.toShort, (2.toShort, (-1.toShort, (-1.toShort, 0.toShort)))))),
        ((1.toShort, 0.toShort), Failure(new ArithmeticException("/ by zero"))),
        ((605.toShort, 7698.toShort), Failure(new ArithmeticException("Short overflow"))),
        ((5094.toShort, -32768.toShort), Failure(new ArithmeticException("Short overflow"))),
        ((5350.toShort, -1.toShort), Success((5349.toShort, (5351.toShort, (-5350.toShort, (-5350.toShort, 0.toShort)))))),
        ((8115.toShort, -32768.toShort), Failure(new ArithmeticException("Short overflow"))),
        ((14217.toShort, 32767.toShort), Failure(new ArithmeticException("Short overflow"))),
        ((16223.toShort, -11686.toShort), Failure(new ArithmeticException("Short overflow"))),
        ((16989.toShort, 1.toShort), Success((16990.toShort, (16988.toShort, (16989.toShort, (16989.toShort, 0.toShort)))))),
        ((20397.toShort, -4450.toShort), Failure(new ArithmeticException("Short overflow"))),
        ((20488.toShort, 1.toShort), Success((20489.toShort, (20487.toShort, (20488.toShort, (20488.toShort, 0.toShort)))))),
        ((32767.toShort, -32768.toShort), Failure(new ArithmeticException("Short overflow"))),
        ((32767.toShort, -13423.toShort), Failure(new ArithmeticException("Short overflow"))),
        ((32767.toShort, 32767.toShort), Failure(new ArithmeticException("Short overflow")))
      ),
      existingFeature(
        { (x: (Short, Short)) =>
          val a = x._1; val b = x._2
          val plus = n.plus(a, b)
          val minus = n.minus(a, b)
          val mul = n.times(a, b)
          val div = (a / b).toShortExact
          val mod = (a % b).toShortExact
          (plus, (minus, (mul, (div, mod))))
        },
        """{ (x: (Short, Short)) =>
         |  val a = x._1; val b = x._2
         |  val plus = a + b
         |  val minus = a - b
         |  val mul = a * b
         |  val div = a / b
         |  val mod = a % b
         |  (plus, (minus, (mul, (div, mod))))
         |}""".stripMargin,
        FuncValue(
          Vector((1, STuple(Vector(SShort, SShort)))),
          BlockValue(
            Vector(
              ValDef(
                3,
                List(),
                SelectField.typed[ShortValue](ValUse(1, STuple(Vector(SShort, SShort))), 1.toByte)
              ),
              ValDef(
                4,
                List(),
                SelectField.typed[ShortValue](ValUse(1, STuple(Vector(SShort, SShort))), 2.toByte)
              )
            ),
            Tuple(
              Vector(
                ArithOp(ValUse(3, SShort), ValUse(4, SShort), OpCode @@ (-102.toByte)),
                Tuple(
                  Vector(
                    ArithOp(ValUse(3, SShort), ValUse(4, SShort), OpCode @@ (-103.toByte)),
                    Tuple(
                      Vector(
                        ArithOp(ValUse(3, SShort), ValUse(4, SShort), OpCode @@ (-100.toByte)),
                        Tuple(
                          Vector(
                            ArithOp(ValUse(3, SShort), ValUse(4, SShort), OpCode @@ (-99.toByte)),
                            ArithOp(ValUse(3, SShort), ValUse(4, SShort), OpCode @@ (-98.toByte))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ))
  }

  property("Short methods equivalence (new features)") {
    lazy val toBytes = newFeature((x: Short) => x.toBytes, "{ (x: Short) => x.toBytes }")
    lazy val toBits = newFeature((x: Short) => x.toBits, "{ (x: Short) => x.toBits }")
    lazy val toAbs = newFeature((x: Short) => x.toAbs, "{ (x: Short) => x.toAbs }")

    lazy val compareTo = newFeature((x: (Short, Short)) => x._1.compareTo(x._2),
      "{ (x: (Short, Short)) => x._1.compareTo(x._2) }")

    lazy val bitOr = newFeature(
    { (x: (Short, Short)) => (x._1 | x._2).toShortExact },
    "{ (x: (Short, Short)) => x._1 | x._2 }")

    lazy val bitAnd = newFeature(
    { (x: (Short, Short)) => (x._1 & x._2).toShortExact },
    "{ (x: (Short, Short)) => x._1 & x._2 }")

    forAll { x: Short =>
      Seq(toBytes, toBits, toAbs).foreach(_.checkEquality(x))
    }
    forAll { x: (Short, Short) =>
      Seq(compareTo, bitOr, bitAnd).foreach(_.checkEquality(x))
    }
  }

  property("Int methods equivalence") {
    testCases(
      Seq(
        (Int.MinValue, Failure(new ArithmeticException("Byte overflow"))),
        (-2014394379, Failure(new ArithmeticException("Byte overflow"))),
        (Byte.MinValue.toInt, Success(Byte.MinValue)),
        (-1, Success(-1.toByte)),
        (0, Success(0.toByte)),
        (1, Success(1.toByte)),
        (Byte.MaxValue.toInt, Success(Byte.MaxValue)),
        (181686429, Failure(new ArithmeticException("Byte overflow"))),
        (Int.MaxValue, Failure(new ArithmeticException("Byte overflow")))
      ),
      existingFeature((x: Int) => x.toByteExact,
        "{ (x: Int) => x.toByte }",
        FuncValue(Vector((1, SInt)), Downcast(ValUse(1, SInt), SByte))))

    testCases(
      Seq(
        (Int.MinValue, Failure(new ArithmeticException("Short overflow"))),
        (Short.MinValue - 1, Failure(new ArithmeticException("Short overflow"))),
        (Short.MinValue.toInt, Success(Short.MinValue)),
        (-1, Success(-1.toShort)),
        (0, Success(0.toShort)),
        (1, Success(1.toShort)),
        (Short.MaxValue.toInt, Success(Short.MaxValue)),
        (Short.MaxValue + 1, Failure(new ArithmeticException("Short overflow"))),
        (Int.MaxValue, Failure(new ArithmeticException("Short overflow")))
      ),
      existingFeature((x: Int) => x.toShortExact,
        "{ (x: Int) => x.toShort }",
        FuncValue(Vector((1, SInt)), Downcast(ValUse(1, SInt), SShort))))

    testCases(
      Seq(
        (Int.MinValue, Success(Int.MinValue)),
        (-1, Success(-1)),
        (0, Success(0)),
        (1, Success(1)),
        (Int.MaxValue, Success(Int.MaxValue))
      ),
      existingFeature((x: Int) => x.toInt,
        "{ (x: Int) => x.toInt }",
        FuncValue(Vector((1, SInt)), ValUse(1, SInt))))

    testCases(
      Seq(
        (Int.MinValue, Success(Int.MinValue.toLong)),
        (-1, Success(-1L)),
        (0, Success(0L)),
        (1, Success(1L)),
        (Int.MaxValue, Success(Int.MaxValue.toLong))
      ),
      existingFeature((x: Int) => x.toLong,
        "{ (x: Int) => x.toLong }",
        FuncValue(Vector((1, SInt)), Upcast(ValUse(1, SInt), SLong))))

    testCases(
      Seq(
        (Int.MinValue, Success(CBigInt(new BigInteger("-80000000", 16)))),
        (-1937187314, Success(CBigInt(new BigInteger("-737721f2", 16)))),
        (-1, Success(CBigInt(new BigInteger("-1", 16)))),
        (0, Success(CBigInt(new BigInteger("0", 16)))),
        (1, Success(CBigInt(new BigInteger("1", 16)))),
        (1542171288, Success(CBigInt(new BigInteger("5bebaa98", 16)))),
        (Int.MaxValue, Success(CBigInt(new BigInteger("7fffffff", 16))))
      ),
      existingFeature((x: Int) => x.toBigInt,
        "{ (x: Int) => x.toBigInt }",
        FuncValue(Vector((1, SInt)), Upcast(ValUse(1, SInt), SBigInt))))

    val n = ExactNumeric.IntIsExactNumeric
    testCases(
    Seq(
      ((Int.MinValue, 449583993), Failure(new ArithmeticException("integer overflow"))),
      ((-1589633733, 2147483647), Failure(new ArithmeticException("integer overflow"))),
      ((-1585471506, -1), Success((-1585471507, (-1585471505, (1585471506, (1585471506, 0)))))),
      ((-1569005179, 1230236634), Failure(new ArithmeticException("integer overflow"))),
      ((-1493733356, -1319619597), Failure(new ArithmeticException("integer overflow"))),
      ((-1100263120, -880052091), Failure(new ArithmeticException("integer overflow"))),
      ((-1055955857, 309147303), Failure(new ArithmeticException("integer overflow"))),
      ((-569807371, 0), Failure(new ArithmeticException("/ by zero"))),
      ((-522264843, 2147483647), Failure(new ArithmeticException("integer overflow"))),
      ((-109552389, 0), Failure(new ArithmeticException("/ by zero"))),
      ((-1, -2147483648), Failure(new ArithmeticException("integer overflow"))),
      ((-1, -1), Success((-2, (0, (1, (1, 0)))))),
      ((-1, 0), Failure(new ArithmeticException("/ by zero"))),
      ((0, -2147483648), Failure(new ArithmeticException("integer overflow"))),
      ((1, -1525049432), Success((-1525049431, (1525049433, (-1525049432, (0, 1)))))),
      ((1, 0), Failure(new ArithmeticException("/ by zero"))),
      ((1, 805353746), Success((805353747, (-805353745, (805353746, (0, 1)))))),
      ((1, 2147483647), Failure(new ArithmeticException("integer overflow"))),
      ((475797978, 0), Failure(new ArithmeticException("/ by zero"))),
      ((782343922, -1448560539), Failure(new ArithmeticException("integer overflow"))),
      ((928769361, 542647292), Failure(new ArithmeticException("integer overflow"))),
      ((1568062151, 0), Failure(new ArithmeticException("/ by zero"))),
      ((1698252401, -1), Success((1698252400, (1698252402, (-1698252401, (-1698252401, 0)))))),
      ((1949795740, -1575667037), Failure(new ArithmeticException("integer overflow"))),
      ((Int.MaxValue, -1), Failure(new ArithmeticException("integer overflow"))),
      ((Int.MaxValue, 1), Failure(new ArithmeticException("integer overflow"))),
      ((Int.MaxValue, 1738276576), Failure(new ArithmeticException("integer overflow")))
    ),
    existingFeature(
      { (x: (Int, Int)) =>
        val a = x._1; val b = x._2
        val plus = n.plus(a, b)
        val minus = n.minus(a, b)
        val mul = n.times(a, b)
        val div = a / b
        val mod = a % b
        (plus, (minus, (mul, (div, mod))))
      },
      """{ (x: (Int, Int)) =>
        |  val a = x._1; val b = x._2
        |  val plus = a + b
        |  val minus = a - b
        |  val mul = a * b
        |  val div = a / b
        |  val mod = a % b
        |  (plus, (minus, (mul, (div, mod))))
        |}""".stripMargin,
        FuncValue(
          Vector((1, STuple(Vector(SInt, SInt)))),
          BlockValue(
            Vector(
              ValDef(
                3,
                List(),
                SelectField.typed[IntValue](ValUse(1, STuple(Vector(SInt, SInt))), 1.toByte)
              ),
              ValDef(
                4,
                List(),
                SelectField.typed[IntValue](ValUse(1, STuple(Vector(SInt, SInt))), 2.toByte)
              )
            ),
            Tuple(
              Vector(
                ArithOp(ValUse(3, SInt), ValUse(4, SInt), OpCode @@ (-102.toByte)),
                Tuple(
                  Vector(
                    ArithOp(ValUse(3, SInt), ValUse(4, SInt), OpCode @@ (-103.toByte)),
                    Tuple(
                      Vector(
                        ArithOp(ValUse(3, SInt), ValUse(4, SInt), OpCode @@ (-100.toByte)),
                        Tuple(
                          Vector(
                            ArithOp(ValUse(3, SInt), ValUse(4, SInt), OpCode @@ (-99.toByte)),
                            ArithOp(ValUse(3, SInt), ValUse(4, SInt), OpCode @@ (-98.toByte))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )))
  }

  property("Int methods equivalence (new features)") {
    lazy val toBytes = newFeature((x: Int) => x.toBytes, "{ (x: Int) => x.toBytes }")
    lazy val toBits = newFeature((x: Int) => x.toBits, "{ (x: Int) => x.toBits }")
    lazy val toAbs = newFeature((x: Int) => x.toAbs, "{ (x: Int) => x.toAbs }")
    lazy val compareTo = newFeature((x: (Int, Int)) => x._1.compareTo(x._2),
      "{ (x: (Int, Int)) => x._1.compareTo(x._2) }")

    lazy val bitOr = newFeature(
    { (x: (Int, Int)) => x._1 | x._2 },
    "{ (x: (Int, Int)) => x._1 | x._2 }")

    lazy val bitAnd = newFeature(
    { (x: (Int, Int)) => x._1 & x._2 },
    "{ (x: (Int, Int)) => x._1 & x._2 }")

    forAll { x: Int =>
      Seq(toBytes, toBits, toAbs).foreach(_.checkEquality(x))
    }
    forAll { x: (Int, Int) =>
      Seq(compareTo, bitOr, bitAnd).foreach(_.checkEquality(x))
    }
  }

  property("Long methods equivalence") {
    testCases(
      Seq(
        (Long.MinValue, Failure(new ArithmeticException("Byte overflow"))),
        (Byte.MinValue.toLong - 1, Failure(new ArithmeticException("Byte overflow"))),
        (Byte.MinValue.toLong, Success(Byte.MinValue)),
        (-1L, Success(-1.toByte)),
        (0L, Success(0.toByte)),
        (1L, Success(1.toByte)),
        (Byte.MaxValue.toLong, Success(Byte.MaxValue)),
        (Byte.MaxValue.toLong + 1, Failure(new ArithmeticException("Byte overflow"))),
        (Long.MinValue, Failure(new ArithmeticException("Byte overflow")))
      ),
      existingFeature((x: Long) => x.toByteExact,
        "{ (x: Long) => x.toByte }",
        FuncValue(Vector((1, SLong)), Downcast(ValUse(1, SLong), SByte))))

    testCases(
      Seq(
        (Long.MinValue, Failure(new ArithmeticException("Short overflow"))),
        (Short.MinValue.toLong - 1, Failure(new ArithmeticException("Short overflow"))),
        (Short.MinValue.toLong, Success(Short.MinValue)),
        (-1L, Success(-1.toShort)),
        (0L, Success(0.toShort)),
        (1L, Success(1.toShort)),
        (Short.MaxValue.toLong, Success(Short.MaxValue)),
        (Short.MaxValue.toLong + 1, Failure(new ArithmeticException("Short overflow"))),
        (Long.MinValue, Failure(new ArithmeticException("Short overflow")))
      ),
      existingFeature((x: Long) => x.toShortExact,
        "{ (x: Long) => x.toShort }",
        FuncValue(Vector((1, SLong)), Downcast(ValUse(1, SLong), SShort))))

    testCases(
      Seq(
        (Long.MinValue, Failure(new ArithmeticException("Int overflow"))),
        (Int.MinValue.toLong - 1, Failure(new ArithmeticException("Int overflow"))),
        (Int.MinValue.toLong, Success(Int.MinValue)),
        (-1L, Success(-1.toInt)),
        (0L, Success(0.toInt)),
        (1L, Success(1.toInt)),
        (Int.MaxValue.toLong, Success(Int.MaxValue)),
        (Int.MaxValue.toLong + 1, Failure(new ArithmeticException("Int overflow"))),
        (Long.MinValue, Failure(new ArithmeticException("Int overflow")))
      ),
      existingFeature((x: Long) => x.toIntExact,
        "{ (x: Long) => x.toInt }",
        FuncValue(Vector((1, SLong)), Downcast(ValUse(1, SLong), SInt))))

    testCases(
      Seq(
        (Long.MinValue, Success(Long.MinValue)),
        (-1L, Success(-1L)),
        (0L, Success(0L)),
        (1L, Success(1L)),
        (Long.MaxValue, Success(Long.MaxValue))
      ),
      existingFeature((x: Long) => x.toLong,
        "{ (x: Long) => x.toLong }",
        FuncValue(Vector((1, SLong)), ValUse(1, SLong))))

    testCases(
      Seq(
        (Long.MinValue, Success(CBigInt(new BigInteger("-8000000000000000", 16)))),
        (-1074651039980347209L, Success(CBigInt(new BigInteger("-ee9ed6d57885f49", 16)))),
        (-1L, Success(CBigInt(new BigInteger("-1", 16)))),
        (0L, Success(CBigInt(new BigInteger("0", 16)))),
        (1L, Success(CBigInt(new BigInteger("1", 16)))),
        (1542942726564696512L, Success(CBigInt(new BigInteger("1569a23c25a951c0", 16)))),
        (Long.MaxValue, Success(CBigInt(new BigInteger("7fffffffffffffff", 16))))
      ),
      existingFeature((x: Long) => x.toBigInt,
        "{ (x: Long) => x.toBigInt }",
        FuncValue(Vector((1, SLong)), Upcast(ValUse(1, SLong), SBigInt))))

    val n = ExactNumeric.LongIsExactNumeric
    testCases(
    Seq(
      ((Long.MinValue, -4677100190307931395L), Failure(new ArithmeticException("long overflow"))),
      ((Long.MinValue, -1L), Failure(new ArithmeticException("long overflow"))),
      ((Long.MinValue, 1L), Failure(new ArithmeticException("long overflow"))),
      ((-9223372036854775808L, 0L), Failure(new ArithmeticException("/ by zero"))),
      ((-5828066432064138816L, 9105034716270510411L), Failure(new ArithmeticException("long overflow"))),
      ((-4564956247298949325L, -1L), Success(
        (-4564956247298949326L, (-4564956247298949324L, (4564956247298949325L, (4564956247298949325L, 0L))))
      )),
      ((-1499553565058783253L, -3237683216870282569L), Failure(new ArithmeticException("long overflow"))),
      ((-1368457031689886112L, 9223372036854775807L), Failure(new ArithmeticException("long overflow"))),
      ((-1L, -4354407074688367443L), Success((-4354407074688367444L, (4354407074688367442L, (4354407074688367443L, (0L, -1L)))))),
      ((-1L, -1L), Success((-2L, (0L, (1L, (1L, 0L)))))),
      ((-1L, 5665019549505434695L), Success((5665019549505434694L, (-5665019549505434696L, (-5665019549505434695L, (0L, -1L)))))),
      ((0L, -1L), Success((-1L, (1L, (0L, (0L, 0L)))))),
      ((0L, 0L), Failure(new ArithmeticException("/ by zero"))),
      ((0L, 2112386634269044172L), Success((2112386634269044172L, (-2112386634269044172L, (0L, (0L, 0L)))))),
      ((2254604056782701370L, -5878231674026236574L), Failure(new ArithmeticException("long overflow"))),
      ((2903872550238813643L, 1L), Success(
        (2903872550238813644L, (2903872550238813642L, (2903872550238813643L, (2903872550238813643L, 0L))))
      )),
      ((5091129735284641762L, -427673944382373638L), Failure(new ArithmeticException("long overflow"))),
      ((6029085020194630780L, 2261786144956037939L), Failure(new ArithmeticException("long overflow"))),
      ((8126382074515995418L, -4746652047588907829L), Failure(new ArithmeticException("long overflow"))),
      ((Long.MaxValue, 1L), Failure(new ArithmeticException("long overflow"))),
      ((Long.MaxValue, -1L), Failure(new ArithmeticException("long overflow")))
    ),
    existingFeature(
      { (x: (Long, Long)) =>
        val a = x._1; val b = x._2
        val plus = n.plus(a, b)
        val minus = n.minus(a, b)
        val mul = n.times(a, b)
        val div = a / b
        val mod = a % b
        (plus, (minus, (mul, (div, mod))))
      },
      """{ (x: (Long, Long)) =>
       |  val a = x._1; val b = x._2
       |  val plus = a + b
       |  val minus = a - b
       |  val mul = a * b
       |  val div = a / b
       |  val mod = a % b
       |  (plus, (minus, (mul, (div, mod))))
       |}""".stripMargin,
      FuncValue(
        Vector((1, STuple(Vector(SLong, SLong)))),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[LongValue](ValUse(1, STuple(Vector(SLong, SLong))), 1.toByte)
            ),
            ValDef(
              4,
              List(),
              SelectField.typed[LongValue](ValUse(1, STuple(Vector(SLong, SLong))), 2.toByte)
            )
          ),
          Tuple(
            Vector(
              ArithOp(ValUse(3, SLong), ValUse(4, SLong), OpCode @@ (-102.toByte)),
              Tuple(
                Vector(
                  ArithOp(ValUse(3, SLong), ValUse(4, SLong), OpCode @@ (-103.toByte)),
                  Tuple(
                    Vector(
                      ArithOp(ValUse(3, SLong), ValUse(4, SLong), OpCode @@ (-100.toByte)),
                      Tuple(
                        Vector(
                          ArithOp(ValUse(3, SLong), ValUse(4, SLong), OpCode @@ (-99.toByte)),
                          ArithOp(ValUse(3, SLong), ValUse(4, SLong), OpCode @@ (-98.toByte))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )))
  }

  property("Long methods equivalence (new features)") {
    lazy val toBytes = newFeature((x: Long) => x.toBytes, "{ (x: Long) => x.toBytes }")
    lazy val toBits = newFeature((x: Long) => x.toBits, "{ (x: Long) => x.toBits }")
    lazy val toAbs = newFeature((x: Long) => x.toAbs, "{ (x: Long) => x.toAbs }")

    lazy val compareTo = newFeature((x: (Long, Long)) => x._1.compareTo(x._2),
      "{ (x: (Long, Long)) => x._1.compareTo(x._2) }")

    lazy val bitOr = newFeature(
    { (x: (Long, Long)) => x._1 | x._2 },
    "{ (x: (Long, Long)) => x._1 | x._2 }")

    lazy val bitAnd = newFeature(
    { (x: (Long, Long)) => x._1 & x._2 },
    "{ (x: (Long, Long)) => x._1 & x._2 }")

    forAll { x: Long =>
      Seq(toBytes, toBits, toAbs).foreach(_.checkEquality(x))
    }
    forAll { x: (Long, Long) =>
      Seq(compareTo, bitOr, bitAnd).foreach(_.checkEquality(x))
    }
  }

  property("BigInt methods equivalence") {
    testCases(
      Seq(
        (CBigInt(new BigInteger("-85102d7f884ca0e8f56193b46133acaf7e4681e1757d03f191ae4f445c8e0", 16)), Success(
          CBigInt(new BigInteger("-85102d7f884ca0e8f56193b46133acaf7e4681e1757d03f191ae4f445c8e0", 16))
        )),
        (CBigInt(new BigInteger("-8000000000000000", 16)), Success(CBigInt(new BigInteger("-8000000000000000", 16)))),
        (CBigInt(new BigInteger("-1", 16)), Success(CBigInt(new BigInteger("-1", 16)))),
        (CBigInt(new BigInteger("0", 16)), Success(CBigInt(new BigInteger("0", 16)))),
        (CBigInt(new BigInteger("1", 16)), Success(CBigInt(new BigInteger("1", 16)))),
        (CBigInt(new BigInteger("7fffffffffffffff", 16)), Success(CBigInt(new BigInteger("7fffffffffffffff", 16)))),
        (CBigInt(new BigInteger("bdd56c22eb3eace8bc4e1c38c65dfdb2e4ffdcf421ae78c36b93b9ff37dc0", 16)), Success(
          CBigInt(new BigInteger("bdd56c22eb3eace8bc4e1c38c65dfdb2e4ffdcf421ae78c36b93b9ff37dc0", 16))
        ))
      ),
      existingFeature((x: BigInt) => x,
        "{ (x: BigInt) => x.toBigInt }",
        FuncValue(Vector((1, SBigInt)), ValUse(1, SBigInt))))

    val n = NumericOps.BigIntIsExactNumeric
    testCases(
    Seq(
      ((CBigInt(new BigInteger("-8683d1cd99d5fcf0e6eff6295c285c36526190e13dbde008c49e5ae6fddc1c", 16)),
        CBigInt(new BigInteger("-2ef55db3f245feddacf0182e299dd", 16))),
          Failure(new ArithmeticException("BigInteger out of 256 bit range"))),

      ((CBigInt(new BigInteger("-68e1136872f98fb0245ec5aa4bef46e16273e860746c892", 16)),
        CBigInt(new BigInteger("-352aaa769b41a327", 16))),
          Failure(new ArithmeticException("BigInteger: modulus not positive"))),

      ((CBigInt(new BigInteger("-39fc00ebf09080cbd8408dd38c4b7490bea533447047140", 16)),
        CBigInt(new BigInteger("31de9e96177dbd39", 16))),
          Success((
            CBigInt(new BigInteger("-39fc00ebf09080cbd8408dd38c4b748da0bb49e2f86b407", 16)),
              (CBigInt(new BigInteger("-39fc00ebf09080cbd8408dd38c4b7493dc8f1ca5e822e79", 16)),
                (CBigInt(new BigInteger("-b4ba8a17d328dac74ef014d7be35597a1259f8b16f0ff1c9820dea23d97740", 16)),
                    (CBigInt(new BigInteger("-129a8045376e104f0d3771b6c2c128fc", 16)),
                     CBigInt(new BigInteger("12fe89836fc97815", 16)))))) )),

      ((CBigInt(new BigInteger("-8000000000000000", 16)), CBigInt(new BigInteger("8000000000000000", 16))),
          Success((
            CBigInt(new BigInteger("0", 16)),
              (CBigInt(new BigInteger("-10000000000000000", 16)),
                (CBigInt(new BigInteger("-40000000000000000000000000000000", 16)),
                    (CBigInt(new BigInteger("-1", 16)), CBigInt(new BigInteger("0", 16)))))) )),

      ((CBigInt(new BigInteger("-47dede8d3e4804bb", 16)), CBigInt(new BigInteger("-388828eb6dfce683", 16))),
          Failure(new ArithmeticException("BigInteger: modulus not positive"))),

      ((CBigInt(new BigInteger("-4fde491150ea00d", 16)), CBigInt(new BigInteger("-80000001", 16))),
          Failure(new ArithmeticException("BigInteger: modulus not positive"))),

      ((CBigInt(new BigInteger("-80000001", 16)), CBigInt(new BigInteger("-80000001", 16))),
          Failure(new ArithmeticException("BigInteger: modulus not positive"))),

      ((CBigInt(new BigInteger("0", 16)), CBigInt(new BigInteger("-8000000000000000", 16))),
          Failure(new ArithmeticException("BigInteger: modulus not positive"))),
          
      ((CBigInt(new BigInteger("0", 16)), CBigInt(new BigInteger("0", 16))),
          Failure(new ArithmeticException("BigInteger divide by zero"))),

      ((CBigInt(new BigInteger("1", 16)),
          CBigInt(new BigInteger("-86063f66e06d6d535c95862cd506309a95d10102422fee", 16))),
          Failure(new ArithmeticException("BigInteger: modulus not positive"))),

      ((CBigInt(new BigInteger("80000000", 16)), CBigInt(new BigInteger("4e592ce5b544b8f7a91f97ec9ea2f2c3660111360297a4", 16))),
          Success((
            CBigInt(new BigInteger("4e592ce5b544b8f7a91f97ec9ea2f2c3660111b60297a4", 16)),
              (CBigInt(new BigInteger("-4e592ce5b544b8f7a91f97ec9ea2f2c3660110b60297a4", 16)),
                (CBigInt(new BigInteger("272c9672daa25c7bd48fcbf64f517961b300889b014bd200000000", 16)),
                    (CBigInt(new BigInteger("0", 16)), CBigInt(new BigInteger("80000000", 16)))))) )),

      ((CBigInt(new BigInteger("3d31398dc4783303", 16)),
        CBigInt(new BigInteger("-37b381db4e6e927e202a2a421d5a09ca", 16))),
          Failure(new ArithmeticException("BigInteger: modulus not positive"))),

      ((CBigInt(new BigInteger("5524814a26357cb71488b6fb26af2d3", 16)),
        CBigInt(new BigInteger("c413b7d975a9972427f46996299fe57cfe79479ac954a7", 16))),
          Failure(new ArithmeticException("BigInteger out of 256 bit range")))
    ),
    existingFeature(
      { (x: (BigInt, BigInt)) =>
        val a = x._1; val b = x._2
        val plus = n.plus(a, b)
        val minus = n.minus(a, b)
        val mul = n.times(a, b)
        val div = a / b
        val mod = a % b
        (plus, (minus, (mul, (div, mod))))
      },
      """{ (x: (BigInt, BigInt)) =>
       |  val a = x._1; val b = x._2
       |  val plus = a + b
       |  val minus = a - b
       |  val mul = a * b
       |  val div = a / b
       |  val mod = a % b
       |  (plus, (minus, (mul, (div, mod))))
       |}""".stripMargin,
      FuncValue(
        Vector((1, STuple(Vector(SBigInt, SBigInt)))),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[BigIntValue](ValUse(1, STuple(Vector(SBigInt, SBigInt))), 1.toByte)
            ),
            ValDef(
              4,
              List(),
              SelectField.typed[BigIntValue](ValUse(1, STuple(Vector(SBigInt, SBigInt))), 2.toByte)
            )
          ),
          Tuple(
            Vector(
              ArithOp(ValUse(3, SBigInt), ValUse(4, SBigInt), OpCode @@ (-102.toByte)),
              Tuple(
                Vector(
                  ArithOp(ValUse(3, SBigInt), ValUse(4, SBigInt), OpCode @@ (-103.toByte)),
                  Tuple(
                    Vector(
                      ArithOp(ValUse(3, SBigInt), ValUse(4, SBigInt), OpCode @@ (-100.toByte)),
                      Tuple(
                        Vector(
                          ArithOp(ValUse(3, SBigInt), ValUse(4, SBigInt), OpCode @@ (-99.toByte)),
                          ArithOp(ValUse(3, SBigInt), ValUse(4, SBigInt), OpCode @@ (-98.toByte))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ), true)
  }

  property("BigInt methods equivalence (new features)") {
    val toByte = newFeature((x: BigInt) => x.toByte,
      "{ (x: BigInt) => x.toByte }",
      FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SByte)))

    val toShort = newFeature((x: BigInt) => x.toShort,
      "{ (x: BigInt) => x.toShort }",
      FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SShort)))

    val toInt = newFeature((x: BigInt) => x.toInt,
      "{ (x: BigInt) => x.toInt }",
      FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SInt)))

    val toLong = newFeature((x: BigInt) => x.toLong,
      "{ (x: BigInt) => x.toLong }",
      FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SLong)))

    lazy val toBytes = newFeature((x: BigInt) => x.toBytes, "{ (x: BigInt) => x.toBytes }")
    lazy val toBits = newFeature((x: BigInt) => x.toBits, "{ (x: BigInt) => x.toBits }")
    lazy val toAbs = newFeature((x: BigInt) => x.toAbs, "{ (x: BigInt) => x.toAbs }")

    lazy val compareTo = newFeature((x: (BigInt, BigInt)) => x._1.compareTo(x._2),
      "{ (x: (BigInt, BigInt)) => x._1.compareTo(x._2) }")

    lazy val bitOr = newFeature({ (x: (BigInt, BigInt)) => x._1 | x._2 },
    "{ (x: (BigInt, BigInt)) => x._1 | x._2 }")

    lazy val bitAnd = newFeature({ (x: (BigInt, BigInt)) => x._1 & x._2 },
    "{ (x: (BigInt, BigInt)) => x._1 & x._2 }")

    forAll { x: BigInt =>
      Seq(toByte, toShort, toInt, toLong, toBytes, toBits, toAbs).foreach(_.checkEquality(x))
    }
    forAll { x: (BigInt, BigInt) =>
      Seq(compareTo, bitOr, bitAnd).foreach(_.checkEquality(x))
    }
  }

  property("GroupElement methods equivalence") {
    val ge1 = "03358d53f01276211f92d0aefbd278805121d4ff6eb534b777af1ee8abae5b2056"
    val ge2 = "02dba7b94b111f3894e2f9120b577da595ec7d58d488485adf73bf4e153af63575"
    val ge3 = "0290449814f5671172dd696a61b8aa49aaa4c87013f56165e27d49944e98bc414d"

    testCases(
      Seq(
        (Helpers.decodeGroupElement(ge1), Success(Helpers.decodeBytes(ge1))),
        (Helpers.decodeGroupElement(ge2), Success(Helpers.decodeBytes(ge2))),
        (Helpers.decodeGroupElement(ge3), Success(Helpers.decodeBytes(ge3))),
        (SigmaDsl.groupGenerator,
          Success(Helpers.decodeBytes("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"))),
        (SigmaDsl.groupIdentity,
          Success(Helpers.decodeBytes("000000000000000000000000000000000000000000000000000000000000000000")))
      ),
      existingFeature((x: GroupElement) => x.getEncoded,
        "{ (x: GroupElement) => x.getEncoded }",
        FuncValue(
          Vector((1, SGroupElement)),
          MethodCall(ValUse(1, SGroupElement), SGroupElement.getMethodByName("getEncoded"), Vector(), Map())
        )))

    testCases(
      Seq(
        (Helpers.decodeGroupElement(ge1), Success(true)),
        (Helpers.decodeGroupElement(ge2), Success(true)),
        (Helpers.decodeGroupElement(ge3), Success(true)),
        (SigmaDsl.groupGenerator, Success(true)),
        (SigmaDsl.groupIdentity, Success(true))
      ),
      existingFeature({ (x: GroupElement) => decodePoint(x.getEncoded) == x },
        "{ (x: GroupElement) => decodePoint(x.getEncoded) == x }",
        FuncValue(
          Vector((1, SGroupElement)),
          EQ(
            DecodePoint(
              MethodCall.typed[Value[SCollection[SByte.type]]](
                ValUse(1, SGroupElement),
                SGroupElement.getMethodByName("getEncoded"),
                Vector(),
                Map()
              )
            ),
            ValUse(1, SGroupElement)
          )
        )))

    testCases(
      Seq(
        (Helpers.decodeGroupElement(ge1), Success(Helpers.decodeGroupElement("02358d53f01276211f92d0aefbd278805121d4ff6eb534b777af1ee8abae5b2056"))),
        (Helpers.decodeGroupElement(ge2), Success(Helpers.decodeGroupElement("03dba7b94b111f3894e2f9120b577da595ec7d58d488485adf73bf4e153af63575"))),
        (Helpers.decodeGroupElement(ge3), Success(Helpers.decodeGroupElement("0390449814f5671172dd696a61b8aa49aaa4c87013f56165e27d49944e98bc414d"))),
        (SigmaDsl.groupGenerator, Success(Helpers.decodeGroupElement("0379be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"))),
        (SigmaDsl.groupIdentity, Success(Helpers.decodeGroupElement("000000000000000000000000000000000000000000000000000000000000000000")))
      ),
      existingFeature({ (x: GroupElement) => x.negate },
        "{ (x: GroupElement) => x.negate }",
        FuncValue(
          Vector((1, SGroupElement)),
          MethodCall(ValUse(1, SGroupElement), SGroupElement.getMethodByName("negate"), Vector(), Map())
        )))

    //TODO HF: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
    // val isIdentity = existingFeature({ (x: GroupElement) => x.isIdentity },
    //   "{ (x: GroupElement) => x.isIdentity }")

    testCases(
      Seq(
        ((Helpers.decodeGroupElement(ge1), CBigInt(new BigInteger("-25c80b560dd7844e2efd10f80f7ee57d", 16))),
            Success(Helpers.decodeGroupElement("023a850181b7b73f92a5bbfa0bfc78f5bbb6ff00645ddde501037017e1a2251e2e"))),
        ((Helpers.decodeGroupElement(ge2), CBigInt(new BigInteger("2488741265082fb02b09f992be3dd8d60d2bbe80d9e2630", 16))),
            Success(Helpers.decodeGroupElement("032045b928fb7774a4cd9ef5fa8209f4e493cd4cc5bd536b52746a53871bf73431"))),
        ((Helpers.decodeGroupElement(ge3), CBigInt(new BigInteger("-33e8fbdb13d2982e92583445e1fdcb5901a178a7aa1e100", 16))),
            Success(Helpers.decodeGroupElement("036128efaf14d8ac2812a662f6494dc617b87986a3dc6b4a59440048a7ac7d2729"))),
        ((Helpers.decodeGroupElement(ge3), CBigInt(new BigInteger("1", 16))),
            Success(Helpers.decodeGroupElement(ge3)))
      ),
      existingFeature({ (x: (GroupElement, BigInt)) => x._1.exp(x._2) },
        "{ (x: (GroupElement, BigInt)) => x._1.exp(x._2) }",
        FuncValue(
          Vector((1, STuple(Vector(SGroupElement, SBigInt)))),
          Exponentiate(
            SelectField.typed[Value[SGroupElement.type]](
              ValUse(1, STuple(Vector(SGroupElement, SBigInt))),
              1.toByte
            ),
            SelectField.typed[Value[SBigInt.type]](
              ValUse(1, STuple(Vector(SGroupElement, SBigInt))),
              2.toByte
            )
          )
        )))

    testCases(
      Seq(
        ((Helpers.decodeGroupElement(ge1), Helpers.decodeGroupElement("03e132ca090614bd6c9f811e91f6daae61f16968a1e6c694ed65aacd1b1092320e")),
            Success(Helpers.decodeGroupElement("02bc48937b4a66f249a32dfb4d2efd0743dc88d46d770b8c5d39fd03325ba211df"))),
        ((Helpers.decodeGroupElement(ge2), Helpers.decodeGroupElement("03e132ca090614bd6c9f811e91f6daae61f16968a1e6c694ed65aacd1b1092320e")),
            Success(Helpers.decodeGroupElement("0359c3bb2ac4ea4dbd7b1e09d7b11198141a3263834fb84a88039629ec1e9311d1"))),
        ((Helpers.decodeGroupElement(ge3), Helpers.decodeGroupElement("03e132ca090614bd6c9f811e91f6daae61f16968a1e6c694ed65aacd1b1092320e")),
            Success(Helpers.decodeGroupElement("02eca42e28548d3fb9fa77cdd0c983066c3ad141ebb086b5044ce46b9ba9b5a714"))),
        ((Helpers.decodeGroupElement(ge3), SigmaDsl.groupIdentity),
            Success(Helpers.decodeGroupElement(ge3)))
      ),
      existingFeature({ (x: (GroupElement, GroupElement)) => x._1.multiply(x._2) },
        "{ (x: (GroupElement, GroupElement)) => x._1.multiply(x._2) }",
        FuncValue(
          Vector((1, STuple(Vector(SGroupElement, SGroupElement)))),
          MultiplyGroup(
            SelectField.typed[Value[SGroupElement.type]](
              ValUse(1, STuple(Vector(SGroupElement, SGroupElement))),
              1.toByte
            ),
            SelectField.typed[Value[SGroupElement.type]](
              ValUse(1, STuple(Vector(SGroupElement, SGroupElement))),
              2.toByte
            )
          )
        )))
  }

  property("AvlTree properties equivalence") {
    def expectedExprFor(propName: String) = {
      FuncValue(
        Vector((1, SAvlTree)),
        MethodCall(
          ValUse(1, SAvlTree),
          SAvlTree.getMethodByName(propName),
          Vector(),
          Map()
        )
      )
    }
    val t1 = CAvlTree(
      AvlTreeData(
        ADDigest @@ ErgoAlgos.decodeUnsafe("000183807f66b301530120ff7fc6bd6601ff01ff7f7d2bedbbffff00187fe89094"),
        AvlTreeFlags(false, true, true),
        1,
        Some(1)
      )
    )
    val t2 = CAvlTree(
      AvlTreeData(
        ADDigest @@ ErgoAlgos.decodeUnsafe("ff000d937f80ffd731ed802d24358001ff8080ff71007f00ad37e0a7ae43fff95b"),
        AvlTreeFlags(false, false, false),
        32,
        Some(64)
      )
    )
    val t3 = CAvlTree(
      AvlTreeData(
        ADDigest @@ ErgoAlgos.decodeUnsafe("3100d2e101ff01fc047c7f6f00ff80129df69a5090012f01ffca99f5bfff0c8036"),
        AvlTreeFlags(true, false, false),
        128,
        None
      )
    )

    testCases(
      Seq(
        (t1, Success(Helpers.decodeBytes("000183807f66b301530120ff7fc6bd6601ff01ff7f7d2bedbbffff00187fe89094"))),
        (t2, Success(Helpers.decodeBytes("ff000d937f80ffd731ed802d24358001ff8080ff71007f00ad37e0a7ae43fff95b"))),
        (t3, Success(Helpers.decodeBytes("3100d2e101ff01fc047c7f6f00ff80129df69a5090012f01ffca99f5bfff0c8036")))
      ),
      existingFeature((t: AvlTree) => t.digest,
        "{ (t: AvlTree) => t.digest }",
        expectedExprFor("digest")))

    testCases(
      Seq(
        (t1, Success(6.toByte)),
        (t2, Success(0.toByte)),
        (t3, Success(1.toByte))
      ),
      existingFeature((t: AvlTree) => t.enabledOperations,
        "{ (t: AvlTree) => t.enabledOperations }",
        expectedExprFor("enabledOperations")))

    testCases(
      Seq(
        (t1, Success(1)),
        (t2, Success(32)),
        (t3, Success(128))
      ),
      existingFeature((t: AvlTree) => t.keyLength,
        "{ (t: AvlTree) => t.keyLength }",
        expectedExprFor("keyLength")))

    testCases(
      Seq(
        (t1, Success(Some(1))),
        (t2, Success(Some(64))),
        (t3, Success(None))
      ),
      existingFeature((t: AvlTree) => t.valueLengthOpt,
        "{ (t: AvlTree) => t.valueLengthOpt }",
        expectedExprFor("valueLengthOpt")))

    testCases(
      Seq(
        (t1, Success(false)),
        (t2, Success(false)),
        (t3, Success(true))
      ),
      existingFeature((t: AvlTree) => t.isInsertAllowed,
        "{ (t: AvlTree) => t.isInsertAllowed }",
        expectedExprFor("isInsertAllowed")))

    testCases(
      Seq(
        (t1, Success(true)),
        (t2, Success(false)),
        (t3, Success(false))
      ),
      existingFeature((t: AvlTree) => t.isUpdateAllowed,
        "{ (t: AvlTree) => t.isUpdateAllowed }",
        expectedExprFor("isUpdateAllowed")))

    testCases(
      Seq(
        (t1, Success(true)),
        (t2, Success(false)),
        (t3, Success(false))
      ),
      existingFeature((t: AvlTree) => t.isRemoveAllowed,
        "{ (t: AvlTree) => t.isRemoveAllowed }",
        expectedExprFor("isRemoveAllowed")))
  }

  property("AvlTree.{contains, get, getMany, updateDigest, updateOperations} equivalence") {
    val contains = existingFeature(
      (t: (AvlTree, (Coll[Byte], Coll[Byte]))) => t._1.contains(t._2._1, t._2._2),
      "{ (t: (AvlTree, (Coll[Byte], Coll[Byte]))) => t._1.contains(t._2._1, t._2._2) }",
      FuncValue(
        Vector(
          (1, STuple(Vector(SAvlTree, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte))))))
        ),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[STuple]](
                ValUse(
                  1,
                  STuple(Vector(SAvlTree, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte)))))
                ),
                2.toByte
              )
            )
          ),
          MethodCall.typed[Value[SBoolean.type]](
            SelectField.typed[Value[SAvlTree.type]](
              ValUse(
                1,
                STuple(Vector(SAvlTree, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte)))))
              ),
              1.toByte
            ),
            SAvlTree.getMethodByName("contains"),
            Vector(
              SelectField.typed[Value[SCollection[SByte.type]]](
                ValUse(3, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte)))),
                1.toByte
              ),
              SelectField.typed[Value[SCollection[SByte.type]]](
                ValUse(3, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte)))),
                2.toByte
              )
            ),
            Map()
          )
        )
      ))

    val get = existingFeature((t: (AvlTree, (Coll[Byte], Coll[Byte]))) => t._1.get(t._2._1, t._2._2),
      "{ (t: (AvlTree, (Coll[Byte], Coll[Byte]))) => t._1.get(t._2._1, t._2._2) }",
      FuncValue(
        Vector(
          (1, STuple(Vector(SAvlTree, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte))))))
        ),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[STuple]](
                ValUse(
                  1,
                  STuple(Vector(SAvlTree, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte)))))
                ),
                2.toByte
              )
            )
          ),
          MethodCall.typed[Value[SOption[SCollection[SByte.type]]]](
            SelectField.typed[Value[SAvlTree.type]](
              ValUse(
                1,
                STuple(Vector(SAvlTree, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte)))))
              ),
              1.toByte
            ),
            SAvlTree.getMethodByName("get"),
            Vector(
              SelectField.typed[Value[SCollection[SByte.type]]](
                ValUse(3, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte)))),
                1.toByte
              ),
              SelectField.typed[Value[SCollection[SByte.type]]](
                ValUse(3, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte)))),
                2.toByte
              )
            ),
            Map()
          )
        )
      ))

    val getMany = existingFeature(
      (t: (AvlTree, (Coll[Coll[Byte]], Coll[Byte]))) => t._1.getMany(t._2._1, t._2._2),
      "{ (t: (AvlTree, (Coll[Coll[Byte]], Coll[Byte]))) => t._1.getMany(t._2._1, t._2._2) }",
      FuncValue(
        Vector(
          (
              1,
              STuple(
                Vector(
                  SAvlTree,
                  STuple(Vector(SCollectionType(SCollectionType(SByte)), SCollectionType(SByte)))
                )
              )
          )
        ),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[STuple]](
                ValUse(
                  1,
                  STuple(
                    Vector(
                      SAvlTree,
                      STuple(Vector(SCollectionType(SCollectionType(SByte)), SCollectionType(SByte)))
                    )
                  )
                ),
                2.toByte
              )
            )
          ),
          MethodCall.typed[Value[SCollection[SOption[SCollection[SByte.type]]]]](
            SelectField.typed[Value[SAvlTree.type]](
              ValUse(
                1,
                STuple(
                  Vector(
                    SAvlTree,
                    STuple(Vector(SCollectionType(SCollectionType(SByte)), SCollectionType(SByte)))
                  )
                )
              ),
              1.toByte
            ),
            SAvlTree.getMethodByName("getMany"),
            Vector(
              SelectField.typed[Value[SCollection[SCollection[SByte.type]]]](
                ValUse(3, STuple(Vector(SCollectionType(SCollectionType(SByte)), SCollectionType(SByte)))),
                1.toByte
              ),
              SelectField.typed[Value[SCollection[SByte.type]]](
                ValUse(3, STuple(Vector(SCollectionType(SCollectionType(SByte)), SCollectionType(SByte)))),
                2.toByte
              )
            ),
            Map()
          )
        )
      )
    )

    val updateDigest = existingFeature((t: (AvlTree, Coll[Byte])) => t._1.updateDigest(t._2),
      "{ (t: (AvlTree, Coll[Byte])) => t._1.updateDigest(t._2) }",
      FuncValue(
        Vector((1, STuple(Vector(SAvlTree, SCollectionType(SByte))))),
        MethodCall.typed[Value[SAvlTree.type]](
          SelectField.typed[Value[SAvlTree.type]](
            ValUse(1, STuple(Vector(SAvlTree, SCollectionType(SByte)))),
            1.toByte
          ),
          SAvlTree.getMethodByName("updateDigest"),
          Vector(
            SelectField.typed[Value[SCollection[SByte.type]]](
              ValUse(1, STuple(Vector(SAvlTree, SCollectionType(SByte)))),
              2.toByte
            )
          ),
          Map()
        )
      ))

    val updateOperations = existingFeature((t: (AvlTree, Byte)) => t._1.updateOperations(t._2),
      "{ (t: (AvlTree, Byte)) => t._1.updateOperations(t._2) }",
      FuncValue(
        Vector((1, STuple(Vector(SAvlTree, SByte)))),
        MethodCall.typed[Value[SAvlTree.type]](
          SelectField.typed[Value[SAvlTree.type]](ValUse(1, STuple(Vector(SAvlTree, SByte))), 1.toByte),
          SAvlTree.getMethodByName("updateOperations"),
          Vector(
            SelectField.typed[Value[SByte.type]](ValUse(1, STuple(Vector(SAvlTree, SByte))), 2.toByte)
          ),
          Map()
        )
      ))

    val (key, value, _, avlProver) = sampleAvlProver
    val otherKey = key.map(x => (-x).toByte) // any other different from key

    val table = Table(("key", "contains", "valueOpt"), (key, true, Some(value)), (otherKey, false, None))
    forAll(table) { (key, okContains, valueOpt) =>
      avlProver.performOneOperation(Lookup(ADKey @@ key.toArray))
      val proof = avlProver.generateProof().toColl
      val digest = avlProver.digest.toColl
      val tree = SigmaDsl.avlTree(AvlTreeFlags.ReadOnly.serializeToByte, digest, 32, None)

      // positive test
      contains.checkExpected((tree, (key, proof)), okContains)
      get.checkExpected((tree, (key, proof)), valueOpt)


      val keys = Colls.fromItems(key)
      val expRes = Colls.fromItems(valueOpt)
      getMany.checkExpected((tree, (keys, proof)), expRes)

      updateDigest.checkEquality((tree, digest)).get.digest shouldBe digest
      val newOps = 1.toByte
      updateOperations.checkEquality((tree, newOps)).get.enabledOperations shouldBe newOps

      { // negative tests: invalid proof
        val invalidProof = proof.map(x => (-x).toByte) // any other different from proof
        val resContains = contains.checkEquality((tree, (key, invalidProof)))
        resContains shouldBe Success(false)
        val resGet = get.checkEquality((tree, (key, invalidProof)))
        resGet.isFailure shouldBe true
        val resGetMany = getMany.checkEquality((tree, (keys, invalidProof)))
        resGetMany.isFailure shouldBe true
      }
    }
  }

  type BatchProver = BatchAVLProver[Digest32, Blake2b256.type]

  def performInsert(avlProver: BatchProver, key: Coll[Byte], value: Coll[Byte]) = {
    avlProver.performOneOperation(Insert(ADKey @@ key.toArray, ADValue @@ value.toArray))
    val proof = avlProver.generateProof().toColl
    proof
  }

  def performUpdate(avlProver: BatchProver, key: Coll[Byte], value: Coll[Byte]) = {
    avlProver.performOneOperation(Update(ADKey @@ key.toArray, ADValue @@ value.toArray))
    val proof = avlProver.generateProof().toColl
    proof
  }

  def performRemove(avlProver: BatchProver, key: Coll[Byte]) = {
    avlProver.performOneOperation(Remove(ADKey @@ key.toArray))
    val proof = avlProver.generateProof().toColl
    proof
  }

  def createTree(digest: Coll[Byte], insertAllowed: Boolean = false, updateAllowed: Boolean = false, removeAllowed: Boolean = false) = {
    val flags = AvlTreeFlags(insertAllowed, updateAllowed, removeAllowed).serializeToByte
    val tree = SigmaDsl.avlTree(flags, digest, 32, None)
    tree
  }

  type KV = (Coll[Byte], Coll[Byte])

  property("AvlTree.insert equivalence") {
    val insert = existingFeature((t: (AvlTree, (Coll[KV], Coll[Byte]))) => t._1.insert(t._2._1, t._2._2),
      "{ (t: (AvlTree, (Coll[(Coll[Byte], Coll[Byte])], Coll[Byte]))) => t._1.insert(t._2._1, t._2._2) }",
      FuncValue(
        Vector(
          (
              1,
              STuple(
                Vector(
                  SAvlTree,
                  STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                )
              )
              )
        ),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[STuple]](
                ValUse(
                  1,
                  STuple(
                    Vector(
                      SAvlTree,
                      STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                    )
                  )
                ),
                2.toByte
              )
            )
          ),
          MethodCall.typed[Value[SOption[SAvlTree.type]]](
            SelectField.typed[Value[SAvlTree.type]](
              ValUse(
                1,
                STuple(
                  Vector(
                    SAvlTree,
                    STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                  )
                )
              ),
              1.toByte
            ),
            SAvlTree.getMethodByName("insert"),
            Vector(
              SelectField.typed[Value[SCollection[STuple]]](
                ValUse(
                  3,
                  STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                ),
                1.toByte
              ),
              SelectField.typed[Value[SCollection[SByte.type]]](
                ValUse(
                  3,
                  STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                ),
                2.toByte
              )
            ),
            Map()
          )
        )
      ))

    forAll(keyCollGen, bytesCollGen) { (key, value) =>
      val (tree, avlProver) = createAvlTreeAndProver()
      val preInsertDigest = avlProver.digest.toColl
      val insertProof = performInsert(avlProver, key, value)
      val kvs = Colls.fromItems((key -> value))

      { // positive
        val preInsertTree = createTree(preInsertDigest, insertAllowed = true)
        val res = insert.checkEquality((preInsertTree, (kvs, insertProof)))
        res.get.isDefined shouldBe true
      }

      { // negative: readonly tree
        val readonlyTree = createTree(preInsertDigest)
        val res = insert.checkEquality((readonlyTree, (kvs, insertProof)))
        res.get.isDefined shouldBe false
      }

      { // negative: invalid key
        val tree = createTree(preInsertDigest, insertAllowed = true)
        val invalidKey = key.map(x => (-x).toByte) // any other different from key
        val invalidKvs = Colls.fromItems((invalidKey -> value)) // NOTE, insertProof is based on `key`
        val res = insert.checkEquality((tree, (invalidKvs, insertProof)))
        res.get.isDefined shouldBe true // TODO HF: should it really be true? (looks like a bug)
      }

      { // negative: invalid proof
        val tree = createTree(preInsertDigest, insertAllowed = true)
        val invalidProof = insertProof.map(x => (-x).toByte) // any other different from proof
        val res = insert.checkEquality((tree, (kvs, invalidProof)))
        res.isFailure shouldBe true
      }
    }
  }

  property("AvlTree.update equivalence") {
    val update = existingFeature((t: (AvlTree, (Coll[KV], Coll[Byte]))) => t._1.update(t._2._1, t._2._2),
      "{ (t: (AvlTree, (Coll[(Coll[Byte], Coll[Byte])], Coll[Byte]))) => t._1.update(t._2._1, t._2._2) }",
      FuncValue(
        Vector(
          (
              1,
              STuple(
                Vector(
                  SAvlTree,
                  STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                )
              )
              )
        ),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[STuple]](
                ValUse(
                  1,
                  STuple(
                    Vector(
                      SAvlTree,
                      STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                    )
                  )
                ),
                2.toByte
              )
            )
          ),
          MethodCall.typed[Value[SOption[SAvlTree.type]]](
            SelectField.typed[Value[SAvlTree.type]](
              ValUse(
                1,
                STuple(
                  Vector(
                    SAvlTree,
                    STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                  )
                )
              ),
              1.toByte
            ),
            SAvlTree.getMethodByName("update"),
            Vector(
              SelectField.typed[Value[SCollection[STuple]]](
                ValUse(
                  3,
                  STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                ),
                1.toByte
              ),
              SelectField.typed[Value[SCollection[SByte.type]]](
                ValUse(
                  3,
                  STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                ),
                2.toByte
              )
            ),
            Map()
          )
        )
      ))

    forAll(keyCollGen, bytesCollGen) { (key, value) =>
      val (_, avlProver) = createAvlTreeAndProver(key -> value)
      val preUpdateDigest = avlProver.digest.toColl
      val newValue = bytesCollGen.sample.get
      val updateProof = performUpdate(avlProver, key, newValue)
      val kvs = Colls.fromItems((key -> newValue))
      val endDigest = avlProver.digest.toColl

      { // positive: update to newValue
        val preUpdateTree = createTree(preUpdateDigest, updateAllowed = true)
        val endTree = preUpdateTree.updateDigest(endDigest)
        update.checkExpected((preUpdateTree, (kvs, updateProof)), Some(endTree))
      }

      { // positive: update to the same value (identity operation)
        val tree = createTree(preUpdateDigest, updateAllowed = true)
        val keys = Colls.fromItems((key -> value))
        update.checkExpected((tree, (keys, updateProof)), Some(tree))
      }

      { // negative: readonly tree
        val readonlyTree = createTree(preUpdateDigest)
        val res = update.checkExpected((readonlyTree, (kvs, updateProof)), None)
      }

      { // negative: invalid key
        val tree = createTree(preUpdateDigest, updateAllowed = true)
        val invalidKey = key.map(x => (-x).toByte) // any other different from key
        val invalidKvs = Colls.fromItems((invalidKey -> newValue))
        update.checkExpected((tree, (invalidKvs, updateProof)), None)
      }

      { // negative: invalid value (different from the value in the proof)
        val tree = createTree(preUpdateDigest, updateAllowed = true)
        val invalidValue = newValue.map(x => (-x).toByte)
        val invalidKvs = Colls.fromItems((key -> invalidValue))
        val res = update.checkEquality((tree, (invalidKvs, updateProof)))
        res.get.isDefined shouldBe true  // TODO HF: should it really be true? (looks like a bug)
      }

      { // negative: invalid proof
        val tree = createTree(preUpdateDigest, updateAllowed = true)
        val invalidProof = updateProof.map(x => (-x).toByte) // any other different from proof
        update.checkExpected((tree, (kvs, invalidProof)), None)
      }
    }
  }

  property("AvlTree.remove equivalence") {
    val remove = existingFeature((t: (AvlTree, (Coll[Coll[Byte]], Coll[Byte]))) => t._1.remove(t._2._1, t._2._2),
      "{ (t: (AvlTree, (Coll[Coll[Byte]], Coll[Byte]))) => t._1.remove(t._2._1, t._2._2) }",
      FuncValue(
        Vector((1, STuple(Vector(SAvlTree, STuple(Vector(SByteArray2, SByteArray)))))),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[STuple]](
                ValUse(1, STuple(Vector(SAvlTree, STuple(Vector(SByteArray2, SByteArray))))),
                2.toByte
              )
            )
          ),
          MethodCall.typed[Value[SOption[SAvlTree.type]]](
            SelectField.typed[Value[SAvlTree.type]](
              ValUse(1, STuple(Vector(SAvlTree, STuple(Vector(SByteArray2, SByteArray))))),
              1.toByte
            ),
            SAvlTree.getMethodByName("remove"),
            Vector(
              SelectField.typed[Value[SCollection[SCollection[SByte.type]]]](
                ValUse(3, STuple(Vector(SByteArray2, SByteArray))),
                1.toByte
              ),
              SelectField.typed[Value[SCollection[SByte.type]]](
                ValUse(3, STuple(Vector(SByteArray2, SByteArray))),
                2.toByte
              )
            ),
            Map()
          )
        )
      ))

    forAll(keyCollGen, bytesCollGen) { (key, value) =>
      val (_, avlProver) = createAvlTreeAndProver(key -> value)
      val preRemoveDigest = avlProver.digest.toColl
      val removeProof = performRemove(avlProver, key)
      val endDigest = avlProver.digest.toColl
      val keys = Colls.fromItems(key)

      { // positive
        val preRemoveTree = createTree(preRemoveDigest, removeAllowed = true)
        val endTree = preRemoveTree.updateDigest(endDigest)
        remove.checkExpected((preRemoveTree, (keys, removeProof)), Some(endTree))
      }

      { // negative: readonly tree
        val readonlyTree = createTree(preRemoveDigest)
        remove.checkExpected((readonlyTree, (keys, removeProof)), None)
      }

      { // negative: invalid key
        val tree = createTree(preRemoveDigest, removeAllowed = true)
        val invalidKey = key.map(x => (-x).toByte) // any other different from `key`
        val invalidKeys = Colls.fromItems(invalidKey)
        remove.checkExpected((tree, (invalidKeys, removeProof)), None)
      }

      { // negative: invalid proof
        val tree = createTree(preRemoveDigest, removeAllowed = true)
        val invalidProof = removeProof.map(x => (-x).toByte) // any other different from `removeProof`
        remove.checkExpected((tree, (keys, invalidProof)), None)
      }
    }
  }

  property("longToByteArray equivalence") {
    testCases(
      Seq(
        (-9223372036854775808L, Success(Helpers.decodeBytes("8000000000000000"))),
        (-1148502660425090565L, Success(Helpers.decodeBytes("f00fb2ea55c579fb"))),
        (-1L, Success(Helpers.decodeBytes("ffffffffffffffff"))),
        (0L, Success(Helpers.decodeBytes("0000000000000000"))),
        (1L, Success(Helpers.decodeBytes("0000000000000001"))),
        (238790047448232028L, Success(Helpers.decodeBytes("03505a48720cf05c"))),
        (9223372036854775807L, Success(Helpers.decodeBytes("7fffffffffffffff")))
      ),
      existingFeature((x: Long) => SigmaDsl.longToByteArray(x),
        "{ (x: Long) => longToByteArray(x) }",
        FuncValue(Vector((1, SLong)), LongToByteArray(ValUse(1, SLong)))))
  }

  property("byteArrayToBigInt equivalence") {
    testCases(
      Seq(
        (Helpers.decodeBytes(""),
            Failure(new NumberFormatException("Zero length BigInteger"))),
        (Helpers.decodeBytes("00"),
            Success(CBigInt(new BigInteger("0", 16)))),
        (Helpers.decodeBytes("01"),
            Success(CBigInt(new BigInteger("1", 16)))),
        (Helpers.decodeBytes("ff"),
            Success(CBigInt(new BigInteger("-1", 16)))),
        (Helpers.decodeBytes("80d6c201"),
            Success(CBigInt(new BigInteger("-7f293dff", 16)))),
        (Helpers.decodeBytes("70d6c201"),
            Success(CBigInt(new BigInteger("70d6c201", 16)))),
        (Helpers.decodeBytes(
          "80e0ff7f02807fff72807f0a00ff7fb7c57f75c11ba2802970fd250052807fc37f6480ffff007fff18eeba44"
        ), Failure(new ArithmeticException("BigInteger out of 256 bit range")))
      ),
      existingFeature((x: Coll[Byte]) => SigmaDsl.byteArrayToBigInt(x),
        "{ (x: Coll[Byte]) => byteArrayToBigInt(x) }",
        FuncValue(Vector((1, SByteArray)), ByteArrayToBigInt(ValUse(1, SByteArray)))))
  }

  property("byteArrayToLong equivalence") {
    testCases(
      Seq(
        (Helpers.decodeBytes(""), Failure(new IllegalArgumentException("array too small: 0 < 8"))),
        (Helpers.decodeBytes("81"), Failure(new IllegalArgumentException("array too small: 1 < 8"))),
        (Helpers.decodeBytes("812d7f00ff807f"), Failure(new IllegalArgumentException("array too small: 7 < 8"))),
        (Helpers.decodeBytes("812d7f00ff807f7f"), Success(-9138508426601529473L)),
        (Helpers.decodeBytes("ffffffffffffffff"), Success(-1L)),
        (Helpers.decodeBytes("0000000000000000"), Success(0L)),
        (Helpers.decodeBytes("0000000000000001"), Success(1L)),
        (Helpers.decodeBytes("712d7f00ff807f7f"), Success(8155314142501175167L)),
        (Helpers.decodeBytes("812d7f00ff807f7f0101018050757f0580ac009680f2ffc1"), Success(-9138508426601529473L))
      ),
      existingFeature((x: Coll[Byte]) => SigmaDsl.byteArrayToLong(x),
        "{ (x: Coll[Byte]) => byteArrayToLong(x) }",
        FuncValue(Vector((1, SByteArray)), ByteArrayToLong(ValUse(1, SByteArray)))))
  }

  // TODO soft-fork: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/427
  // TODO costing: expression t._1(t._2) cannot be costed because t is lambda argument
  //  ignore("Func context variable") {
  //    val doApply = checkEq(func[(Int => Int, Int), Int]("{ (t: (Int => Int, Int)) => t._1(t._2) }")) { (t: (Int => Int, Int)) => t._1(t._2) }
  //    val code = compileWithCosting(emptyEnv, s"{ (x: Int) => x + 1 }")
  //    val ctx = ErgoLikeContext.dummy(fakeSelf)
  //    doApply((CFunc[Int, Int](ctx, code), 10))
  //  }

  lazy val ctx = ergoCtx.toSigmaContext(IR, false)

  property("Box properties equivalence") {
    val b1 = CostingBox(
      false,
      new ErgoBox(
        9223372036854775807L,
        new ErgoTree(
          16.toByte,
          Array(
            SigmaPropConstant(
              CSigmaProp(
                ProveDlog(
                  Helpers.decodeECPoint(
                    "0297c44a12f4eb99a85d298fa3ba829b5b42b9f63798c980ece801cc663cc5fc9e"
                  )
                )
              )
            )
          ),
          Right(ConstantPlaceholder(0, SSigmaProp))
        ),
        Coll(
          (Digest32 @@ (ErgoAlgos.decodeUnsafe("6e789ab7b2fffff12280a6cd01557f6fb22b7f80ff7aff8e1f7f15973d7f0001")),
              10000000L),
          (Digest32 @@ (ErgoAlgos.decodeUnsafe("a3ff007f00057600808001ff8f8000019000ffdb806fff7cc0b6015eb37fa600")),
              500L)
        ),
        Map(
          ErgoBox.R5 -> ByteArrayConstant(Helpers.decodeBytes("7fc87f7f01ff")),
          ErgoBox.R4 -> FalseLeaf
        ),
        ModifierId @@ ("218301ae8000018008637f0021fb9e00018055486f0b514121016a00ff718080"),
        22588.toShort,
        677407
      )
    )

    val b2 = CostingBox(
      false,
      new ErgoBox(
        12345L,
        new ErgoTree(
          0.toByte,
          Vector(),
          Right(
            BoolToSigmaProp(
              AND(
                ConcreteCollection(
                  Array(
                    FalseLeaf,
                    XorOf(
                      ConcreteCollection(Array(EQ(IntConstant(1), IntConstant(1)), FalseLeaf), SBoolean)
                    )
                  ),
                  SBoolean
                )
              )
            )
          )
        ),
        Coll(),
        Map(
          ErgoBox.R5 -> ByteArrayConstant(
            Helpers.decodeBytes(
              "297000800b80f1d56c809a8c6affbed864b87f007f6f007f00ac00018c01c4fdff011088807f0100657f00f9ab0101ff6d65"
            )
          ),
          ErgoBox.R4 -> TrueLeaf,
          ErgoBox.R7 -> LongConstant(9223372036854775807L),
          ErgoBox.R6 -> LongConstant(2115927197107005906L)
        ),
        ModifierId @@ ("003bd5c630803cfff6c1ff7f7fb980ff136afc011f8080b8b04ad4dbda2d7f4e"),
        1.toShort,
        1000000
      )
    )

    testCases(
      Seq(
        (b1, Success(Helpers.decodeBytes("5ee78f30ae4e770e44900a46854e9fecb6b12e8112556ef1cd19aef633b4421e"))),
        (b2, Success(Helpers.decodeBytes("3a0089be265460e29ca47d26e5b55a6f3e3ffaf5b4aed941410a2437913848ad")))
      ),
      existingFeature({ (x: Box) => x.id },
        "{ (x: Box) => x.id }",
        FuncValue(Vector((1, SBox)), ExtractId(ValUse(1, SBox)))))

    testCases(
      Seq(
        (b1, Success(9223372036854775807L)),
        (b2, Success(12345L))
      ),
      existingFeature({ (x: Box) => x.value },
        "{ (x: Box) => x.value }",
        FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))))

    testCases(
      Seq(
        (b1, Success(Helpers.decodeBytes(
          "100108cd0297c44a12f4eb99a85d298fa3ba829b5b42b9f63798c980ece801cc663cc5fc9e7300"
        ))),
        (b2, Success(Helpers.decodeBytes("00d1968302010100ff83020193040204020100")))
      ),
      existingFeature({ (x: Box) => x.propositionBytes },
        "{ (x: Box) => x.propositionBytes }",
        FuncValue(Vector((1, SBox)), ExtractScriptBytes(ValUse(1, SBox)))))

    testCases(
      Seq(
        (b1, Success(Helpers.decodeBytes(
          "ffffffffffffffff7f100108cd0297c44a12f4eb99a85d298fa3ba829b5b42b9f63798c980ece801cc663cc5fc9e73009fac29026e789ab7b2fffff12280a6cd01557f6fb22b7f80ff7aff8e1f7f15973d7f000180ade204a3ff007f00057600808001ff8f8000019000ffdb806fff7cc0b6015eb37fa600f4030201000e067fc87f7f01ff218301ae8000018008637f0021fb9e00018055486f0b514121016a00ff718080bcb001"
        ))),
        (b2, Success(Helpers.decodeBytes(
          "b96000d1968302010100ff83020193040204020100c0843d000401010e32297000800b80f1d56c809a8c6affbed864b87f007f6f007f00ac00018c01c4fdff011088807f0100657f00f9ab0101ff6d6505a4a7b5a2e7a4a4dd3a05feffffffffffffffff01003bd5c630803cfff6c1ff7f7fb980ff136afc011f8080b8b04ad4dbda2d7f4e01"
        )))
      ),
      existingFeature({ (x: Box) => x.bytes },
        "{ (x: Box) => x.bytes }",
        FuncValue(Vector((1, SBox)), ExtractBytes(ValUse(1, SBox)))))

    testCases(
      Seq(
        (b1, Success(Helpers.decodeBytes(
          "ffffffffffffffff7f100108cd0297c44a12f4eb99a85d298fa3ba829b5b42b9f63798c980ece801cc663cc5fc9e73009fac29026e789ab7b2fffff12280a6cd01557f6fb22b7f80ff7aff8e1f7f15973d7f000180ade204a3ff007f00057600808001ff8f8000019000ffdb806fff7cc0b6015eb37fa600f4030201000e067fc87f7f01ff"
        ))),
        (b2, Success(Helpers.decodeBytes(
          "b96000d1968302010100ff83020193040204020100c0843d000401010e32297000800b80f1d56c809a8c6affbed864b87f007f6f007f00ac00018c01c4fdff011088807f0100657f00f9ab0101ff6d6505a4a7b5a2e7a4a4dd3a05feffffffffffffffff01"
        )))
      ),
      existingFeature({ (x: Box) => x.bytesWithoutRef },
        "{ (x: Box) => x.bytesWithoutRef }",
        FuncValue(Vector((1, SBox)), ExtractBytesWithNoRef(ValUse(1, SBox)))))

    testCases(
      Seq(
        (b1, Success((
            677407,
            Helpers.decodeBytes("218301ae8000018008637f0021fb9e00018055486f0b514121016a00ff718080583c")
            ))),
        (b2, Success((
            1000000,
            Helpers.decodeBytes("003bd5c630803cfff6c1ff7f7fb980ff136afc011f8080b8b04ad4dbda2d7f4e0001")
            )))
      ),
      existingFeature({ (x: Box) => x.creationInfo },
        "{ (x: Box) => x.creationInfo }",
        FuncValue(Vector((1, SBox)), ExtractCreationInfo(ValUse(1, SBox)))))

    // TODO HF: fix collections equality and remove map(identity)
    //  (PairOfColl should be equal CollOverArray)
    testCases(
      Seq(
        (b1, Success(Coll[(Coll[Byte], Long)](
            (Helpers.decodeBytes("6e789ab7b2fffff12280a6cd01557f6fb22b7f80ff7aff8e1f7f15973d7f0001"), 10000000L),
            (Helpers.decodeBytes("a3ff007f00057600808001ff8f8000019000ffdb806fff7cc0b6015eb37fa600"), 500L)
          ).map(identity))
        ),
        (b2, Success(Coll[(Coll[Byte], Long)]().map(identity)))
      ),
      existingFeature({ (x: Box) => x.tokens },
        "{ (x: Box) => x.tokens }",
        FuncValue(
          Vector((1, SBox)),
          MethodCall.typed[Value[SCollection[STuple]]](
            ValUse(1, SBox),
            SBox.getMethodByName("tokens"),
            Vector(),
            Map()
          )
        )))

  }

  property("Box properties equivalence (new features)") {
    // TODO HF: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/416
    val getReg = newFeature((x: Box) => x.getReg[Int](1).get,
      "{ (x: Box) => x.getReg[Int](1).get }")

    forAll { box: Box =>
      Seq(getReg).foreach(_.checkEquality(box))
    }
  }

  property("Advanced Box test") {
    val (tree, _) = createAvlTreeAndProver()

    val box1 = SigmaDsl.Box(ErgoBox(20, TrueProp, 0, Seq(), Map(
      ErgoBox.R4 -> ByteConstant(1.toByte),
      ErgoBox.R5 -> ShortConstant(1024.toShort),
      ErgoBox.R6 -> IntConstant(1024 * 1024),
      ErgoBox.R7 -> LongConstant(1024.toLong),
      ErgoBox.R8 -> BigIntConstant(222L),
      ErgoBox.R9 -> AvlTreeConstant(tree)
    )))

    val box2 = SigmaDsl.Box(ErgoBox(20, TrueProp, 0, Seq(), Map(
      ErgoBox.R4 -> ByteArrayConstant(Coll(1.toByte))
    )))

    testCases(
      Seq(
        (box1, Success(1.toByte)),
        (box2, Failure(new InvalidType("Cannot getReg[Byte](4): invalid type of value Value(Coll(1)) at id=4")))
      ),
      existingFeature((x: Box) => x.R4[Byte].get,
        "{ (x: Box) => x.R4[Byte].get }",
        FuncValue(
          Vector((1, SBox)),
          OptionGet(ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R4, SOption(SByte)))
        )))

    testCases(
      Seq(
        (box1, Success(1024.toShort)),
        (box2, Failure(new NoSuchElementException("None.get")))
      ),
      existingFeature((x: Box) => x.R5[Short].get,
        "{ (x: Box) => x.R5[Short].get }",
        FuncValue(
          Vector((1, SBox)),
          OptionGet(ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R5, SOption(SShort)))
        )))

    testCases(
      Seq(
        (box1, Success(1024 * 1024))
      ),
      existingFeature((x: Box) => x.R6[Int].get,
        "{ (x: Box) => x.R6[Int].get }",
        FuncValue(
          Vector((1, SBox)),
          OptionGet(ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R6, SOption(SInt)))
        )))

    testCases(
      Seq(
        (box1, Success(1024.toLong))
      ),
      existingFeature((x: Box) => x.R7[Long].get,
        "{ (x: Box) => x.R7[Long].get }",
        FuncValue(
          Vector((1, SBox)),
          OptionGet(ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R7, SOption(SLong)))
        )))

    testCases(
      Seq(
        (box1, Success(CBigInt(BigInteger.valueOf(222L))))
      ),
      existingFeature((x: Box) => x.R8[BigInt].get,
        "{ (x: Box) => x.R8[BigInt].get }",
        FuncValue(
          Vector((1, SBox)),
          OptionGet(ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R8, SOption(SBigInt)))
        )))

    testCases(
      Seq(
        (box1, Success(CAvlTree(
          AvlTreeData(
            ADDigest @@ (
                ErgoAlgos.decodeUnsafe("4ec61f485b98eb87153f7c57db4f5ecd75556fddbc403b41acf8441fde8e160900")
                ),
            AvlTreeFlags(true, true, true),
            32,
            None
          )
        )))
      ),
      existingFeature((x: Box) => x.R9[AvlTree].get,
        "{ (x: Box) => x.R9[AvlTree].get }",
        FuncValue(
          Vector((1, SBox)),
          OptionGet(ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R9, SOption(SAvlTree)))
        )))
  }

  def existingPropTest[A: RType, B: RType](propName: String, scalaFunc: A => B) = {
    val tA = RType[A]
    val typeName = tA.name
    val stypeA = Evaluation.rtypeToSType(tA)
    val typeCompanion = stypeA.asInstanceOf[STypeCompanion]
    existingFeature(scalaFunc,
      s"{ (x: $typeName) => x.$propName }",
      FuncValue(Vector((1, stypeA)),
        MethodCall(ValUse(1, stypeA), typeCompanion.getMethodByName(propName), Vector(), Map() )
      ))
  }

  property("PreHeader properties equivalence") {
    val h = ctx.preHeader
    val version = existingPropTest("version", { (x: PreHeader) => x.version })
    val parentId = existingPropTest("parentId", { (x: PreHeader) => x.parentId })
    val timestamp = existingPropTest("timestamp", { (x: PreHeader) => x.timestamp })
    val nBits = existingPropTest("nBits", { (x: PreHeader) => x.nBits })
    val height = existingPropTest("height", { (x: PreHeader) => x.height })
    val minerPk = existingPropTest("minerPk", { (x: PreHeader) => x.minerPk })
    val votes = existingPropTest("votes", { (x: PreHeader) => x.votes })
    Seq(version, parentId, timestamp, nBits, height, minerPk, votes).foreach(_.checkEquality(h))
  }

  property("Header properties equivalence") {
    val h = ctx.headers(0)
    val id = existingPropTest("id", { (x: Header) => x.id })
    val version = existingPropTest("version", { (x: Header) => x.version })
    val parentId = existingPropTest("parentId", { (x: Header) => x.parentId })
    val ASDProofsRoot = existingPropTest("ADProofsRoot", { (x: Header) => x.ADProofsRoot})
    val stateRoot = existingPropTest("stateRoot", { (x: Header) => x.stateRoot })
    val transactionsRoot = existingPropTest("transactionsRoot", { (x: Header) => x.transactionsRoot })
    val timestamp = existingPropTest("timestamp", { (x: Header) => x.timestamp })
    val nBits = existingPropTest("nBits", { (x: Header) => x.nBits })
    val height = existingPropTest("height", { (x: Header) => x.height })
    val extensionRoot = existingPropTest("extensionRoot", { (x: Header) => x.extensionRoot })
    val minerPk = existingPropTest("minerPk", { (x: Header) => x.minerPk })
    val powOnetimePk = existingPropTest("powOnetimePk", { (x: Header) => x.powOnetimePk })
    val powNonce = existingPropTest("powNonce", { (x: Header) => x.powNonce })
    val powDistance = existingPropTest("powDistance", { (x: Header) => x.powDistance })
    val votes = existingPropTest("votes", { (x: Header) => x.votes })

    Seq(id, version, parentId, ASDProofsRoot, stateRoot, transactionsRoot,
      timestamp, nBits, height, extensionRoot, minerPk, powOnetimePk,
      powNonce, powDistance, votes).foreach(_.checkEquality(h))
  }

  property("Context properties equivalence") {
    val eq = EqualityChecker(ctx)
    val dataInputs = existingPropTest("dataInputs", { (x: Context) => x.dataInputs })

    val dataInputs0 = existingFeature({ (x: Context) => x.dataInputs(0) },
      "{ (x: Context) => x.dataInputs(0) }",
      FuncValue(
        Vector((1, SContext)),
        ByIndex(
          MethodCall.typed[Value[SCollection[SBox.type]]](
            ValUse(1, SContext),
            SContext.getMethodByName("dataInputs"),
            Vector(),
            Map()
          ),
          IntConstant(0),
          None
        )
      ))

    val dataInputs0id = existingFeature({ (x: Context) => x.dataInputs(0).id },
      "{ (x: Context) => x.dataInputs(0).id }",
      FuncValue(
        Vector((1, SContext)),
        ExtractId(
          ByIndex(
            MethodCall.typed[Value[SCollection[SBox.type]]](
              ValUse(1, SContext),
              SContext.getMethodByName("dataInputs"),
              Vector(),
              Map()
            ),
            IntConstant(0),
            None
          )
        )
      ))

    val preHeader = existingPropTest("preHeader", { (x: Context) => x.preHeader })
    val headers = existingPropTest("headers", { (x: Context) => x.headers })

    val outputs = existingFeature({ (x: Context) => x.OUTPUTS },
      "{ (x: Context) => x.OUTPUTS }", FuncValue(Vector((1, SContext)), Outputs))
    val inputs = existingFeature({ (x: Context) => x.INPUTS },
      "{ (x: Context) => x.INPUTS }", FuncValue(Vector((1, SContext)), Inputs))
    val height = existingFeature({ (x: Context) => x.HEIGHT },
      "{ (x: Context) => x.HEIGHT }", FuncValue(Vector((1, SContext)), Height))
    val self = existingFeature({ (x: Context) => x.SELF },
      "{ (x: Context) => x.SELF }", FuncValue(Vector((1, SContext)), Self))
    val inputsMap =  existingFeature(
      { (x: Context) => x.INPUTS.map { (b: Box) => b.value } },
      "{ (x: Context) => x.INPUTS.map { (b: Box) => b.value } }",
      FuncValue(
        Vector((1, SContext)),
        MapCollection(Inputs, FuncValue(Vector((3, SBox)), ExtractAmount(ValUse(3, SBox))))
      ))

    val inputsMap2 = existingFeature(
      { (x: Context) => x.INPUTS.map { (b: Box) => (b.value, b.value) } },
      """{ (x: Context) =>
       |  x.INPUTS.map { (b: Box) => (b.value, b.value) }
       |}""".stripMargin,
      FuncValue(
        Vector((1, SContext)),
        MapCollection(
          Inputs,
          FuncValue(
            Vector((3, SBox)),
            BlockValue(
              Vector(ValDef(5, List(), ExtractAmount(ValUse(3, SBox)))),
              Tuple(Vector(ValUse(5, SLong), ValUse(5, SLong)))
            )
          )
        )
      ))


    val inputsMap3 = existingFeature(
      { (x: Context) =>
        x.INPUTS.map { (b: Box) =>
          val pk = b.R4[Int].get
          val value = longToByteArray(b.value)
          (pk, value)
        }
      },
      """{ (x: Context) =>
       |  x.INPUTS.map { (b: Box) =>
       |    val pk = b.R4[Int].get
       |    val value = longToByteArray(b.value)
       |    (pk, value)
       |  }
       |}""".stripMargin,
      FuncValue(
        Vector((1, SContext)),
        MapCollection(
          Inputs,
          FuncValue(
            Vector((3, SBox)),
            Tuple(
              Vector(
                OptionGet(ExtractRegisterAs(ValUse(3, SBox), ErgoBox.R4, SOption(SInt))),
                LongToByteArray(ExtractAmount(ValUse(3, SBox)))
              )
            )
          )
        )
      ))

    val selfBoxIndex = existingFeature({ (x: Context) => x.selfBoxIndex },
      "{ (x: Context) => x.selfBoxIndex }",
      FuncValue(
        Vector((1, SContext)),
        MethodCall.typed[Value[SInt.type]](
          ValUse(1, SContext),
          SContext.getMethodByName("selfBoxIndex"),
          Vector(),
          Map()
        )
      ))
    ctx.selfBoxIndex shouldBe -1 // TODO HF: see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/603

    val rootHash = existingPropTest("LastBlockUtxoRootHash", { (x: Context) => x.LastBlockUtxoRootHash })

    val rootHashFlag = existingFeature(
      { (x: Context) => x.LastBlockUtxoRootHash.isUpdateAllowed },
      "{ (x: Context) => x.LastBlockUtxoRootHash.isUpdateAllowed }",
      FuncValue(
        Vector((1, SContext)),
        MethodCall.typed[Value[SBoolean.type]](
          MethodCall.typed[Value[SAvlTree.type]](
            ValUse(1, SContext),
            SContext.getMethodByName("LastBlockUtxoRootHash"),
            Vector(),
            Map()
          ),
          SAvlTree.getMethodByName("isUpdateAllowed"),
          Vector(),
          Map()
        )
      ))

    val minerPubKey = existingPropTest("minerPubKey", { (x: Context) => x.minerPubKey })

    val getVar = existingFeature((x: Context) => x.getVar[Int](2).get,
    "{ (x: Context) => getVar[Int](2).get }",
      FuncValue(Vector((1, SContext)), OptionGet(GetVar(2.toByte, SOption(SInt)))))

    Seq(
      dataInputs, dataInputs0, dataInputs0id, preHeader, headers,
      outputs, inputs, height, self, inputsMap, inputsMap2, inputsMap3, selfBoxIndex,
      rootHash, rootHashFlag, minerPubKey, getVar).foreach(_.checkEquality(ctx))
  }

  property("xorOf equivalence") {
    val xor = existingFeature((x: Coll[Boolean]) => SigmaDsl.xorOf(x),
      "{ (x: Coll[Boolean]) => xorOf(x) }",
      FuncValue(Vector((1, SBooleanArray)), XorOf(ValUse(1, SBooleanArray))))
    forAll { x: Array[Boolean] =>
      xor.checkEquality(Colls.fromArray(x))
    }
  }

  property("LogicalNot equivalence") {
    val not = existingFeature((x: Boolean) => !x,
      "{ (x: Boolean) => !x }",
      FuncValue(Vector((1, SBoolean)), LogicalNot(ValUse(1, SBoolean))))
    forAll { x: Boolean => not.checkEquality(x) }
  }

  property("Numeric Negation equivalence") {
    val negByte = existingFeature((x: Byte) => (-x).toByte,
      "{ (x: Byte) => -x }",
      FuncValue(Vector((1, SByte)), Negation(ValUse(1, SByte))))
    forAll { x: Byte => negByte.checkEquality(x) }

    val negShort = existingFeature((x: Short) => (-x).toShort,
      "{ (x: Short) => -x }",
      FuncValue(Vector((1, SShort)), Negation(ValUse(1, SShort))))
    forAll { x: Short => negShort.checkEquality(x) }

    val negInt = existingFeature((x: Int) => -x,
      "{ (x: Int) => -x }",
      FuncValue(Vector((1, SInt)), Negation(ValUse(1, SInt))))
    forAll { x: Int => negInt.checkEquality(x) }

    val negLong = existingFeature((x: Long) => -x,
      "{ (x: Long) => -x }",
      FuncValue(Vector((1, SLong)), Negation(ValUse(1, SLong))))
    forAll { x: Long => negLong.checkEquality(x) }

    val negBigInt = existingFeature((x: BigInt) => x.negate(),
      "{ (x: BigInt) => -x }",
      FuncValue(Vector((1, SBigInt)), Negation(ValUse(1, SBigInt))))
    forAll { x: BigInt => negBigInt.checkEquality(x) }
  }

  property("global functions equivalence") {

    val groupGenerator = existingFeature({ (x: Int) => SigmaDsl.groupGenerator },
      "{ (x: Int) => groupGenerator }",
      FuncValue(
        Vector((1, SInt)),
        MethodCall.typed[Value[SGroupElement.type]](
          Global,
          SGlobal.getMethodByName("groupGenerator"),
          Vector(),
          Map()
        )
      ))

    val groupGenerator2 = existingFeature({ (x: Int) => SigmaDsl.groupGenerator },
      "{ (x: Int) => Global.groupGenerator }",
      FuncValue(
        Vector((1, SInt)),
        MethodCall.typed[Value[SGroupElement.type]](
          Global,
          SGlobal.getMethodByName("groupGenerator"),
          Vector(),
          Map()
        )
      ))

    forAll { dummyValue: Int =>
      Seq(groupGenerator, groupGenerator2).foreach(_.checkEquality(dummyValue))
    }


    val exp = existingFeature({ (n: BigInt) => SigmaDsl.groupGenerator.exp(n) },
      "{ (n: BigInt) => groupGenerator.exp(n) }",
      FuncValue(
        Vector((1, SBigInt)),
        Exponentiate(
          MethodCall.typed[Value[SGroupElement.type]](
            Global,
            SGlobal.getMethodByName("groupGenerator"),
            Vector(),
            Map()
          ),
          ValUse(1, SBigInt)
        )
      ))

    forAll { n: BigInt =>
      exp.checkEquality(n)
    }

    val xor = existingFeature((x: (Coll[Byte], Coll[Byte])) => SigmaDsl.xor(x._1, x._2),
      "{ (x: (Coll[Byte], Coll[Byte])) => xor(x._1, x._2) }",
      FuncValue(
        Vector((1, STuple(Vector(SByteArray, SByteArray)))),
        Xor(
          SelectField.typed[Value[SCollection[SByte.type]]](
            ValUse(1, STuple(Vector(SByteArray, SByteArray))),
            1.toByte
          ),
          SelectField.typed[Value[SCollection[SByte.type]]](
            ValUse(1, STuple(Vector(SByteArray, SByteArray))),
            2.toByte
          )
        )
      ))
    forAll { x: (Coll[Byte], Coll[Byte]) =>
      xor.checkEquality(x)
    }
  }

  property("Coll[Box] methods equivalence") {

    val filter = existingFeature({ (x: Coll[Box]) => x.filter({ (b: Box) => b.value > 1 }) },
      "{ (x: Coll[Box]) => x.filter({(b: Box) => b.value > 1 }) }",
      FuncValue(
        Vector((1, SCollectionType(SBox))),
        Filter(
          ValUse(1, SCollectionType(SBox)),
          FuncValue(Vector((3, SBox)), GT(ExtractAmount(ValUse(3, SBox)), LongConstant(1L)))
        )
      ))

    val flatMap = existingFeature({ (x: Coll[Box]) => x.flatMap({ (b: Box) => b.propositionBytes }) },
      "{ (x: Coll[Box]) => x.flatMap({(b: Box) => b.propositionBytes }) }",
      FuncValue(
        Vector((1, SCollectionType(SBox))),
        MethodCall.typed[Value[SCollection[SByte.type]]](
          ValUse(1, SCollectionType(SBox)),
          SCollection.getMethodByName("flatMap").withConcreteTypes(
            Map(STypeVar("IV") -> SBox, STypeVar("OV") -> SByte)
          ),
          Vector(FuncValue(Vector((3, SBox)), ExtractScriptBytes(ValUse(3, SBox)))),
          Map()
        )
      ))

    val zip = existingFeature({ (x: Coll[Box]) => x.zip(x) },
      "{ (x: Coll[Box]) => x.zip(x) }",
      FuncValue(
        Vector((1, SCollectionType(SBox))),
        MethodCall.typed[Value[SCollection[STuple]]](
          ValUse(1, SCollectionType(SBox)),
          SCollection.getMethodByName("zip").withConcreteTypes(
            Map(STypeVar("IV") -> SBox, STypeVar("OV") -> SBox)
          ),
          Vector(ValUse(1, SCollectionType(SBox))),
          Map()
        )
      ))

    val size = existingFeature({ (x: Coll[Box]) => x.size },
      "{ (x: Coll[Box]) => x.size }",
      FuncValue(Vector((1, SCollectionType(SBox))), SizeOf(ValUse(1, SCollectionType(SBox)))))

    val indices = existingFeature({ (x: Coll[Box]) => x.indices },
      "{ (x: Coll[Box]) => x.indices }",
      FuncValue(
        Vector((1, SCollectionType(SBox))),
        MethodCall.typed[Value[SCollection[SInt.type]]](
          ValUse(1, SCollectionType(SBox)),
          SCollection.getMethodByName("indices").withConcreteTypes(Map(STypeVar("IV") -> SBox)),
          Vector(),
          Map()
        )
      ))

    val forall = existingFeature({ (x: Coll[Box]) => x.forall({ (b: Box) => b.value > 1 }) },
      "{ (x: Coll[Box]) => x.forall({(b: Box) => b.value > 1 }) }",
      FuncValue(
        Vector((1, SCollectionType(SBox))),
        ForAll(
          ValUse(1, SCollectionType(SBox)),
          FuncValue(Vector((3, SBox)), GT(ExtractAmount(ValUse(3, SBox)), LongConstant(1L)))
        )
      ))

    val exists = existingFeature({ (x: Coll[Box]) => x.exists({ (b: Box) => b.value > 1 }) },
      "{ (x: Coll[Box]) => x.exists({(b: Box) => b.value > 1 }) }",
      FuncValue(
        Vector((1, SCollectionType(SBox))),
        Exists(
          ValUse(1, SCollectionType(SBox)),
          FuncValue(Vector((3, SBox)), GT(ExtractAmount(ValUse(3, SBox)), LongConstant(1L)))
        )
      ))
      
    forAll(collOfN[Box](10)) { boxes: Coll[Box] =>
      Seq(filter, flatMap, zip, size, indices, forall, exists).foreach(_.checkEquality(boxes))
    }
  }

  val collWithRangeGen = for {
    arr <- collGen[Int];
    l <- Gen.choose(0, arr.length - 1);
    r <- Gen.choose(l, arr.length - 1) } yield (arr, (l, r))

  property("Coll patch method equivalence") {
    val patch = existingFeature(
      { (x: (Coll[Int], (Int, Int))) =>
        val coll = x._1
        val l = x._2._1; val r = x._2._2
        coll.patch(l, coll, r)
      },
      """{ (x: (Coll[Int], (Int, Int))) =>
        |  val coll = x._1
        |  val l = x._2._1; val r = x._2._2
        |  coll.patch(l, coll, r)
        |}""".stripMargin,
      FuncValue(
        Vector((1, SPair(SCollectionType(SInt), SPair(SInt, SInt)))),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[SCollection[SInt.type]]](
                ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
                1.toByte
              )
            ),
            ValDef(
              4,
              List(),
              SelectField.typed[Value[STuple]](
                ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
                2.toByte
              )
            )
          ),
          MethodCall.typed[Value[SCollection[SInt.type]]](
            ValUse(3, SCollectionType(SInt)),
            SCollection.getMethodByName("patch").withConcreteTypes(Map(STypeVar("IV") -> SInt)),
            Vector(
              SelectField.typed[Value[SInt.type]](ValUse(4, SPair(SInt, SInt)), 1.toByte),
              ValUse(3, SCollectionType(SInt)),
              SelectField.typed[Value[SInt.type]](ValUse(4, SPair(SInt, SInt)), 2.toByte)
            ),
            Map()
          )
        )
      ))

    forAll(collWithRangeGen) { data =>
      val arr = data._1
      val range = data._2
      whenever (arr.length > 1) {
        patch.checkEquality(data)
      }
    }
  }

  property("Coll updated method equivalence") {
    val updated = existingFeature(
      (x: (Coll[Int], (Int, Int))) => x._1.updated(x._2._1, x._2._2),
      "{ (x: (Coll[Int], (Int, Int))) => x._1.updated(x._2._1, x._2._2) }",
      FuncValue(
        Vector((1, SPair(SCollectionType(SInt), SPair(SInt, SInt)))),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[STuple]](
                ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
                2.toByte
              )
            )
          ),
          MethodCall.typed[Value[SCollection[SInt.type]]](
            SelectField.typed[Value[SCollection[SInt.type]]](
              ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
              1.toByte
            ),
            SCollection.getMethodByName("updated").withConcreteTypes(Map(STypeVar("IV") -> SInt)),
            Vector(
              SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SInt, SInt)), 1.toByte),
              SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SInt, SInt)), 2.toByte)
            ),
            Map()
          )
        )
      ))
    forAll { x: (Array[Int], Int) =>
      val size = x._1.size
      whenever (size > 1) {
        val index = getArrayIndex(size)
        updated.checkEquality((Colls.fromArray(x._1), (index, x._2)))
      }
    }
  }

  property("Coll updateMany method equivalence") {
    val updateMany = existingFeature(
      (x: (Coll[Int], (Coll[Int], Coll[Int]))) => x._1.updateMany(x._2._1, x._2._2),
      "{ (x: (Coll[Int], (Coll[Int], Coll[Int]))) => x._1.updateMany(x._2._1, x._2._2) }",
      FuncValue(
        Vector((1, SPair(SCollectionType(SInt), SPair(SCollectionType(SInt), SCollectionType(SInt))))),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[STuple]](
                ValUse(
                  1,
                  SPair(SCollectionType(SInt), SPair(SCollectionType(SInt), SCollectionType(SInt)))
                ),
                2.toByte
              )
            )
          ),
          MethodCall.typed[Value[SCollection[SInt.type]]](
            SelectField.typed[Value[SCollection[SInt.type]]](
              ValUse(1, SPair(SCollectionType(SInt), SPair(SCollectionType(SInt), SCollectionType(SInt)))),
              1.toByte
            ),
            SCollection.getMethodByName("updateMany").withConcreteTypes(Map(STypeVar("IV") -> SInt)),
            Vector(
              SelectField.typed[Value[SCollection[SInt.type]]](
                ValUse(3, SPair(SCollectionType(SInt), SCollectionType(SInt))),
                1.toByte
              ),
              SelectField.typed[Value[SCollection[SInt.type]]](
                ValUse(3, SPair(SCollectionType(SInt), SCollectionType(SInt))),
                2.toByte
              )
            ),
            Map()
          )
        )
      ))

    val dataGen = for {
      arr <- arrayGen[Int];
      len <- Gen.choose(0, arr.length)
      indices <- Gen.containerOfN[Array, Int](len, Gen.choose(0, arr.length - 1))
      } yield (Colls.fromArray(arr), Colls.fromArray(indices))

    forAll(dataGen) { data =>
      val coll = data._1
      val indices = data._2
      whenever (coll.length > 1) {
        val vs = indices.reverse
        val input = (coll, (indices, vs))
        updateMany.checkEquality(input)
      }
    }
  }

  // TODO HF: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
  property("Coll find method equivalence") {
    val find = newFeature((x: Coll[Int]) => x.find({ (v: Int) => v > 0 }),
      "{ (x: Coll[Int]) => x.find({ (v: Int) => v > 0} ) }")
    forAll { x: Coll[Int] =>
      find.checkEquality(x)
    }
  }

  // TODO HF: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/418
  property("Coll bitwise methods equivalence") {
    val shiftRight = newFeature(
      { (x: Coll[Boolean]) =>
        if (x.size > 2) x.slice(0, x.size - 2) else Colls.emptyColl[Boolean]
      },
      "{ (x: Coll[Boolean]) => x >> 2 }")
    forAll { x: Array[Boolean] =>
      shiftRight.checkEquality(Colls.fromArray(x))
    }
  }

  // TODO HF: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
  property("Coll diff methods equivalence") {
    val diff = newFeature((x: (Coll[Int], Coll[Int])) => x._1.diff(x._2),
      "{ (x: (Coll[Int], Coll[Int])) => x._1.diff(x._2) }")
    forAll { (x: Coll[Int], y: Coll[Int]) =>
      diff.checkEquality((x, y))
    }
  }

  property("Coll fold method equivalence") {
    val fold = existingFeature(
      { (x: (Coll[Byte], Int)) => x._1.foldLeft(x._2, { i: (Int, Byte) => i._1 + i._2 }) },
      "{ (x: (Coll[Byte], Int)) => x._1.fold(x._2, { (i1: Int, i2: Byte) => i1 + i2 }) }",
      FuncValue(
        Vector((1, SPair(SByteArray, SInt))),
        Fold(
          SelectField.typed[Value[SCollection[SByte.type]]](ValUse(1, SPair(SByteArray, SInt)), 1.toByte),
          SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SByteArray, SInt)), 2.toByte),
          FuncValue(
            Vector((3, SPair(SInt, SByte))),
            ArithOp(
              SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SInt, SByte)), 1.toByte),
              Upcast(SelectField.typed[Value[SByte.type]](ValUse(3, SPair(SInt, SByte)), 2.toByte), SInt),
              OpCode @@ (-102.toByte)
            )
          )
        )
      ))

    val indexOf = existingFeature(
      { (x: (Coll[Byte], (Byte, Int))) => x._1.indexOf(x._2._1, x._2._2) },
      "{ (x: (Coll[Byte], (Byte, Int))) => x._1.indexOf(x._2._1, x._2._2) }",
      FuncValue(
        Vector((1, SPair(SByteArray, SPair(SByte, SInt)))),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[STuple]](ValUse(1, SPair(SByteArray, SPair(SByte, SInt))), 2.toByte)
            )
          ),
          MethodCall.typed[Value[SInt.type]](
            SelectField.typed[Value[SCollection[SByte.type]]](
              ValUse(1, SPair(SByteArray, SPair(SByte, SInt))),
              1.toByte
            ),
            SCollection.getMethodByName("indexOf").withConcreteTypes(Map(STypeVar("IV") -> SByte)),
            Vector(
              SelectField.typed[Value[SByte.type]](ValUse(3, SPair(SByte, SInt)), 1.toByte),
              SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SByte, SInt)), 2.toByte)
            ),
            Map()
          )
        )
      ))

    forAll { (coll: Coll[Byte], start: Short, value: Byte, from: Int) =>
      fold.checkEquality((coll, start))
      indexOf.checkEquality((coll, (value, from)))
    }
  }

  property("Coll apply method equivalence") {
    val apply = existingFeature((x: (Coll[Int], Int)) => x._1(x._2),
      "{ (x: (Coll[Int], Int)) => x._1(x._2) }",
      FuncValue(
        Vector((1, SPair(SCollectionType(SInt), SInt))),
        ByIndex(
          SelectField.typed[Value[SCollection[SInt.type]]](
            ValUse(1, SPair(SCollectionType(SInt), SInt)),
            1.toByte
          ),
          SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SCollectionType(SInt), SInt)), 2.toByte),
          None
        )
      ))

    forAll { (x: Coll[Int], i: Int) =>
      apply.checkEquality((x, i))
    }
  }

  property("Coll getOrElse method equivalence") {
    val getOrElse = existingFeature((x: (Coll[Int], (Int, Int))) => x._1.getOrElse(x._2._1, x._2._2),
      "{ (x: (Coll[Int], (Int, Int))) => x._1.getOrElse(x._2._1, x._2._2) }",
      FuncValue(
        Vector((1, SPair(SCollectionType(SInt), SPair(SInt, SInt)))),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[STuple]](
                ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
                2.toByte
              )
            )
          ),
          ByIndex(
            SelectField.typed[Value[SCollection[SInt.type]]](
              ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
              1.toByte
            ),
            SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SInt, SInt)), 1.toByte),
            Some(SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SInt, SInt)), 2.toByte))
          )
        )
      ))

    forAll { x: (Coll[Int], (Int, Int)) =>
      getOrElse.checkEquality(x)
    }
  }

  property("Tuple size method equivalence") {
    val size = existingFeature((x: (Int, Int)) => 2,
      "{ (x: (Int, Int)) => x.size }",
      FuncValue(Vector((1, SPair(SInt, SInt))), IntConstant(2)))
    forAll { x: (Int, Int) =>
      size.checkExpected(x, 2)
    }
  }

  property("Tuple apply method equivalence") {
    val apply1 = existingFeature((x: (Int, Int)) => x._1,
      "{ (x: (Int, Int)) => x(0) }",
      FuncValue(
        Vector((1, SPair(SInt, SInt))),
        SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SInt, SInt)), 1.toByte)
      ))
    val apply2 = existingFeature((x: (Int, Int)) => x._2,
      "{ (x: (Int, Int)) => x(1) }",
      FuncValue(
        Vector((1, SPair(SInt, SInt))),
        SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SInt, SInt)), 2.toByte)
      ))
    forAll { x: (Int, Int) =>
      apply1.checkExpected(x, x._1)
      apply2.checkExpected(x, x._2)
    }
  }

  property("Coll map method equivalence") {
    val n = ExactNumeric.IntIsExactNumeric
    val map = existingFeature((x: Coll[Int]) => x.map({ (v: Int) => n.plus(v, 1) }),
      "{ (x: Coll[Int]) => x.map({ (v: Int) => v + 1 }) }")
    forAll { x: Coll[Int] =>
      map.checkEquality(x)
    }
  }

  property("Coll slice method equivalence") {
    val slice = existingFeature((x: (Coll[Int], (Int, Int))) => x._1.slice(x._2._1, x._2._2),
      "{ (x: (Coll[Int], (Int, Int))) => x._1.slice(x._2._1, x._2._2) }",
      FuncValue(
        Vector((1, SPair(SCollectionType(SInt), SPair(SInt, SInt)))),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[STuple]](
                ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
                2.toByte
              )
            )
          ),
          Slice(
            SelectField.typed[Value[SCollection[SInt.type]]](
              ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
              1.toByte
            ),
            SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SInt, SInt)), 1.toByte),
            SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SInt, SInt)), 2.toByte)
          )
        )
      ))
    forAll(collWithRangeGen) { x =>
      slice.checkEquality(x)
    }
  }

  property("Coll append method equivalence") {
    val append = existingFeature(
      { (x: (Coll[Int], (Int, Int))) =>
        val sliced: Coll[Int] = x._1.slice(x._2._1, x._2._2)
        val toAppend: Coll[Int] = x._1
        sliced.append(toAppend)
      },
      """{ (x: (Coll[Int], (Int, Int))) =>
        |val sliced: Coll[Int] = x._1.slice(x._2._1, x._2._2)
        |val toAppend: Coll[Int] = x._1
        |sliced.append(toAppend)
        |}""".stripMargin,
      FuncValue(
        Vector((1, SPair(SCollectionType(SInt), SPair(SInt, SInt)))),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[SCollection[SInt.type]]](
                ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
                1.toByte
              )
            ),
            ValDef(
              4,
              List(),
              SelectField.typed[Value[STuple]](
                ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
                2.toByte
              )
            )
          ),
          Append(
            Slice(
              ValUse(3, SCollectionType(SInt)),
              SelectField.typed[Value[SInt.type]](ValUse(4, SPair(SInt, SInt)), 1.toByte),
              SelectField.typed[Value[SInt.type]](ValUse(4, SPair(SInt, SInt)), 2.toByte)
            ),
            ValUse(3, SCollectionType(SInt))
          )
        )
      ))

    forAll(collWithRangeGen) { x =>
      append.checkEquality(x)
    }
  }

  property("Option methods equivalence") {
    val get = existingFeature({ (x: Option[Long]) => x.get },
      "{ (x: Option[Long]) => x.get }",
      FuncValue(Vector((1, SOption(SLong))), OptionGet(ValUse(1, SOption(SLong)))))

    val isDefined = existingFeature({ (x: Option[Long]) => x.isDefined },
      "{ (x: Option[Long]) => x.isDefined }",
      FuncValue(Vector((1, SOption(SLong))), OptionIsDefined(ValUse(1, SOption(SLong)))))

    val getOrElse = existingFeature({ (x: Option[Long]) => x.getOrElse(1L) },
      "{ (x: Option[Long]) => x.getOrElse(1L) }",
      FuncValue(Vector((1, SOption(SLong))), OptionGetOrElse(ValUse(1, SOption(SLong)), LongConstant(1L))))

    val filter = existingFeature({ (x: Option[Long]) => x.filter({ (v: Long) => v == 1} ) },
      "{ (x: Option[Long]) => x.filter({ (v: Long) => v == 1 }) }",
      FuncValue(
        Vector((1, SOption(SLong))),
        MethodCall.typed[Value[SOption[SLong.type]]](
          ValUse(1, SOption(SLong)),
          SOption.getMethodByName("filter").withConcreteTypes(Map(STypeVar("T") -> SLong)),
          Vector(FuncValue(Vector((3, SLong)), EQ(ValUse(3, SLong), LongConstant(1L)))),
          Map()
        )
      ))

    val n = ExactNumeric.LongIsExactNumeric
    val map = existingFeature({ (x: Option[Long]) => x.map( (v: Long) => n.plus(v, 1) ) },
      "{ (x: Option[Long]) => x.map({ (v: Long) => v + 1 }) }",
      FuncValue(
        Vector((1, SOption(SLong))),
        MethodCall.typed[Value[SOption[SLong.type]]](
          ValUse(1, SOption(SLong)),
          SOption.getMethodByName("map").withConcreteTypes(
            Map(STypeVar("T") -> SLong, STypeVar("R") -> SLong)
          ),
          Vector(
            FuncValue(
              Vector((3, SLong)),
              ArithOp(ValUse(3, SLong), LongConstant(1L), OpCode @@ (-102.toByte))
            )
          ),
          Map()
        )
      ))

    val isEmpty = newFeature({ (x: Option[Long]) => x.isEmpty },
      "{ (x: Option[Long]) => x.isEmpty }")

    forAll { x: Option[Long] =>
      Seq(get, isDefined, getOrElse, filter, map, isEmpty).foreach(_.checkEquality(x))
    }
  }

  // TODO HF: implement Option.fold
  property("Option fold method") {
    val n = ExactNumeric.LongIsExactNumeric
    val fold = newFeature({ (x: Option[Long]) => x.fold(5.toLong)( (v: Long) => n.plus(v, 1) ) },
      "{ (x: Option[Long]) => x.fold(5, { (v: Long) => v + 1 }) }")
    forAll { x: Option[Long] =>
      fold.checkEquality(x)
    }
  }

  property("Option fold workaround method") {
    val n = ExactNumeric.LongIsExactNumeric
    val foldWorkaround = existingFeature({ (x: Option[Long]) => x.fold(5.toLong)( (v: Long) => n.plus(v, 1) ) },
      """{(x: Option[Long]) =>
        |  def f(opt: Long): Long = opt + 1
        |  if (x.isDefined) f(x.get) else 5L
        |}""".stripMargin,
      FuncValue(
        Vector((1, SOption(SLong))),
        If(
          OptionIsDefined(ValUse(1, SOption(SLong))),
          Apply(
            FuncValue(
              Vector((3, SLong)),
              ArithOp(ValUse(3, SLong), LongConstant(1L), OpCode @@ (-102.toByte))
            ),
            Array(OptionGet(ValUse(1, SOption(SLong))))
          ),
          LongConstant(5L)
        )
      ))
    forAll { x: Option[Long] =>
      foldWorkaround.checkEquality(x)
    }
  }

  property("blake2b256, sha256 equivalence") {
    val blake2b256 = existingFeature((x: Coll[Byte]) => SigmaDsl.blake2b256(x),
      "{ (x: Coll[Byte]) => blake2b256(x) }",
      FuncValue(Vector((1, SByteArray)), CalcBlake2b256(ValUse(1, SByteArray))))

    val sha256 = existingFeature((x: Coll[Byte]) => SigmaDsl.sha256(x),
      "{ (x: Coll[Byte]) => sha256(x) }",
      FuncValue(Vector((1, SByteArray)), CalcSha256(ValUse(1, SByteArray))))

    forAll { x: Coll[Byte] =>
      Seq(blake2b256, sha256).foreach(_.checkEquality(x))
    }
  }

  property("print") {
    println(ComplexityTableStat.complexityTableString)
  }

  property("sigmaProp equivalence") {
    lazy val eq = existingFeature((x: Boolean) => sigmaProp(x),
     "{ (x: Boolean) => sigmaProp(x) }",
      FuncValue(Vector((1, SBoolean)), BoolToSigmaProp(ValUse(1, SBoolean))))
    forAll { x: Boolean => eq.checkEquality(x) }
  }

  property("atLeast equivalence") {
    lazy val atLeast = existingFeature((x: Coll[SigmaProp]) => SigmaDsl.atLeast(x.size - 1, x),
      "{ (x: Coll[SigmaProp]) => atLeast(x.size - 1, x) }",
      FuncValue(
        Vector((1, SCollectionType(SSigmaProp))),
        AtLeast(
          ArithOp(SizeOf(ValUse(1, SCollectionType(SSigmaProp))), IntConstant(1), OpCode @@ (-103.toByte)),
          ValUse(1, SCollectionType(SSigmaProp))
        )
      ))
    forAll(collGen[SigmaProp]) { x: Coll[SigmaProp] =>
      atLeast.checkEquality(x)
    }
  }

  property("&& sigma equivalence") {
    lazy val SigmaAnd1 = existingFeature(
      (x: (SigmaProp, SigmaProp)) => x._1 && x._2,
      "{ (x:(SigmaProp, SigmaProp)) => x._1 && x._2 }",
      FuncValue(
        Vector((1, SPair(SSigmaProp, SSigmaProp))),
        SigmaAnd(
          Seq(
            SelectField.typed[Value[SSigmaProp.type]](ValUse(1, SPair(SSigmaProp, SSigmaProp)), 1.toByte),
            SelectField.typed[Value[SSigmaProp.type]](ValUse(1, SPair(SSigmaProp, SSigmaProp)), 2.toByte)
          )
        )
      ))
    lazy val SigmaAnd2 = existingFeature(
      (x: (SigmaProp, Boolean)) => x._1 && sigmaProp(x._2),
      "{ (x:(SigmaProp, Boolean)) => x._1 && sigmaProp(x._2) }",
      FuncValue(
        Vector((1, SPair(SSigmaProp, SBoolean))),
        SigmaAnd(
          Seq(
            SelectField.typed[Value[SSigmaProp.type]](ValUse(1, SPair(SSigmaProp, SBoolean)), 1.toByte),
            BoolToSigmaProp(
              SelectField.typed[Value[SBoolean.type]](ValUse(1, SPair(SSigmaProp, SBoolean)), 2.toByte)
            )
          )
        )
      ))

    forAll { x: (SigmaProp, SigmaProp) =>
      SigmaAnd1.checkEquality(x)
    }
    forAll { x: (SigmaProp, Boolean) =>
      SigmaAnd2.checkEquality(x)
    }
  }

  property("|| sigma equivalence") {
    lazy val SigmaOr1 = existingFeature(
      (x: (SigmaProp, SigmaProp)) => x._1 || x._2,
      "{ (x:(SigmaProp, SigmaProp)) => x._1 || x._2 }",
      FuncValue(
        Vector((1, SPair(SSigmaProp, SSigmaProp))),
        SigmaOr(
          Seq(
            SelectField.typed[SigmaPropValue](ValUse(1, SPair(SSigmaProp, SSigmaProp)), 1.toByte),
            SelectField.typed[SigmaPropValue](ValUse(1, SPair(SSigmaProp, SSigmaProp)), 2.toByte)
          )
        )
      ))
    lazy val SigmaOr2 = existingFeature(
      (x: (SigmaProp, Boolean)) => x._1 || sigmaProp(x._2),
      "{ (x:(SigmaProp, Boolean)) => x._1 || sigmaProp(x._2) }",
      FuncValue(
        Vector((1, SPair(SSigmaProp, SBoolean))),
        SigmaOr(
          Seq(
            SelectField.typed[SigmaPropValue](ValUse(1, SPair(SSigmaProp, SBoolean)), 1.toByte),
            BoolToSigmaProp(
              SelectField.typed[BoolValue](ValUse(1, SPair(SSigmaProp, SBoolean)), 2.toByte)
            )
          )
        )
      ))

    forAll { x: (SigmaProp, SigmaProp) =>
      SigmaOr1.checkEquality(x)
    }
    forAll { x: (SigmaProp, Boolean) =>
      SigmaOr2.checkEquality(x)
    }
  }

  property("SigmaProp.propBytes equivalence") {
    lazy val propBytes = existingFeature((x: SigmaProp) => x.propBytes,
      "{ (x: SigmaProp) => x.propBytes }",
      FuncValue(Vector((1, SSigmaProp)), SigmaPropBytes(ValUse(1, SSigmaProp))))
    forAll { x: SigmaProp =>
      propBytes.checkEquality(x)
    }
  }

  // TODO HF: implement allZK func https://github.com/ScorexFoundation/sigmastate-interpreter/issues/543
  property("allZK equivalence") {
    lazy val allZK = newFeature((x: Coll[SigmaProp]) => SigmaDsl.allZK(x),
      "{ (x: Coll[SigmaProp]) => allZK(x) }")
    forAll { x: Coll[SigmaProp] =>
      allZK.checkEquality(x)
    }
  }

  // TODO HF: implement anyZK func https://github.com/ScorexFoundation/sigmastate-interpreter/issues/543
  property("anyZK equivalence") {
    lazy val anyZK = newFeature((x: Coll[SigmaProp]) => SigmaDsl.anyZK(x),
      "{ (x: Coll[SigmaProp]) => anyZK(x) }")
    forAll { x: Coll[SigmaProp] =>
      anyZK.checkEquality(x)
    }
  }

  property("allOf equivalence") {
    lazy val allOf = existingFeature((x: Coll[Boolean]) => SigmaDsl.allOf(x),
      "{ (x: Coll[Boolean]) => allOf(x) }",
      FuncValue(Vector((1, SBooleanArray)), AND(ValUse(1, SBooleanArray))))

    forAll { x: Coll[Boolean] =>
      allOf.checkEquality(x)
    }
  }

  property("anyOf equivalence") {
    lazy val anyOf = existingFeature((x: Coll[Boolean]) => SigmaDsl.anyOf(x),
      "{ (x: Coll[Boolean]) => anyOf(x) }",
      FuncValue(Vector((1, SBooleanArray)), OR(ValUse(1, SBooleanArray))))

    forAll { x: Coll[Boolean] =>
      anyOf.checkEquality(x)
    }
  }

  property("proveDlog equivalence") {
    val proveDlog = existingFeature({ (x: GroupElement) => SigmaDsl.proveDlog(x) },
      "{ (x: GroupElement) => proveDlog(x) }",
      FuncValue(Vector((1, SGroupElement)), CreateProveDlog(ValUse(1, SGroupElement))))
    forAll { x: GroupElement =>
      proveDlog.checkEquality(x)
    }
  }

  property("proveDHTuple equivalence") {
    val proveDHTuple = existingFeature({ (x: GroupElement) => SigmaDsl.proveDHTuple(x, x, x, x) },
      "{ (x: GroupElement) => proveDHTuple(x, x, x, x) }",
      FuncValue(
        Vector((1, SGroupElement)),
        CreateProveDHTuple(
          ValUse(1, SGroupElement),
          ValUse(1, SGroupElement),
          ValUse(1, SGroupElement),
          ValUse(1, SGroupElement)
        )
      ))
    forAll { x: GroupElement =>
      proveDHTuple.checkEquality(x)
    }
  }

}
