package special.sigma

import java.math.BigInteger

import org.ergoplatform.ErgoScriptPredef.TrueProp
import org.ergoplatform.dsl.{SigmaContractSyntax, TestContractSpec}
import org.ergoplatform._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{PropSpec, Matchers, Tag}
import scalan.{ExactNumeric, RType}
import org.scalactic.source
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Digest32, Blake2b256}
import scalan.util.Extensions._
import sigma.util.Extensions._
import sigmastate.SCollection.SByteArray
import sigmastate.Values.IntConstant
import sigmastate._
import sigmastate.Values._
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.helpers.SigmaPPrint
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.Terms.MethodCall
import sigmastate.utxo.{ComplexityTableStat, SizeOf, SelectField}
import special.collection.Coll
import sigmastate.serialization.OpCodes.OpCode

import scala.util.{DynamicVariable, Success, Failure, Try}


/** This suite tests every method of every SigmaDsl type to be equivalent to
  * the evaluation of the corresponding ErgoScript operation */
class SigmaDslSpec extends SigmaDslTesting { suite =>

  ///=====================================================
  ///              Boolean type operations
  ///-----------------------------------------------------

  property("Boolean methods equivalence") {
    val feature = newFeature((x: Boolean) => x.toByte, "{ (x: Boolean) => x.toByte }")

    val data = Table(("input", "res"),
      (true, 1.toByte),
      (false, 0.toByte)
    )

    forAll(data) { (x, res) =>
      feature.checkExpected(x, res)
    }
  }

  property("BinXor(logical XOR) equivalence") {
    val eq = existingFeature((x: (Boolean, Boolean)) => x._1 ^ x._2,
      "{ (x: (Boolean, Boolean)) => x._1 ^ x._2 }",
      FuncValue(
        Vector((1, STuple(Vector(SBoolean, SBoolean)))),
        BinXor(
          SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SBoolean, SBoolean))), 1.toByte),
          SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SBoolean, SBoolean))), 2.toByte)
        )
      ))
    forAll { x: (Boolean, Boolean) => eq.checkEquality(x) }
  }

  property("BinXor(logical XOR) test") {
    val eq = existingFeature((x: (Int, Boolean)) => (x._1 == 0) ^ x._2,
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
    forAll { x: (Int, Boolean) => eq.checkEquality(x) }
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
    forAll { x: (Boolean, Boolean) => eq.checkEquality(x) }
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
    forAll { x: (Boolean, Boolean) => eq.checkEquality(x) }
  }

  property("lazy || and && boolean equivalence") {
    val features = Seq(
      existingFeature((x: Boolean) => x || (1 / 0 == 1),
        "{ (x: Boolean) => x || (1 / 0 == 1) }",
        FuncValue(
          Vector((1, SBoolean)),
          BinOr(
            ValUse(1, SBoolean),
            EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
          )
        )),

      existingFeature((x: Boolean) => x && (1 / 0 == 1),
        "{ (x: Boolean) => x && (1 / 0 == 1) }",
        FuncValue(
          Vector((1, SBoolean)),
          BinAnd(
            ValUse(1, SBoolean),
            EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
          )
        )),

      // nested

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
        )),

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
        )),

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
        )),

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
        )),

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
        )),

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
        )),

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
        )),

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
        ))
    )
    forAll { x: Boolean =>
      features.foreach(_.checkEquality(x))
    }
  }

  ///=====================================================
  ///              Byte type operations
  ///-----------------------------------------------------

  property("Byte methods equivalence") {
    val toByte = existingFeature(
      (x: Byte) => x.toByte, "{ (x: Byte) => x.toByte }",
      FuncValue(Vector((1, SByte)), ValUse(1, SByte)))

    val toShort = existingFeature(
      (x: Byte) => x.toShort, "{ (x: Byte) => x.toShort }",
      FuncValue(Vector((1, SByte)), Upcast(ValUse(1, SByte), SShort)))

    val toInt = existingFeature(
      (x: Byte) => x.toInt, "{ (x: Byte) => x.toInt }",
      FuncValue(Vector((1, SByte)), Upcast(ValUse(1, SByte), SInt)))

    val toLong = existingFeature(
      (x: Byte) => x.toLong, "{ (x: Byte) => x.toLong }",
      FuncValue(Vector((1, SByte)), Upcast(ValUse(1, SByte), SLong)))

    val toBigInt = existingFeature(
      (x: Byte) => x.toBigInt, "{ (x: Byte) => x.toBigInt }",
      FuncValue(Vector((1, SByte)), Upcast(ValUse(1, SByte), SBigInt)))

    lazy val toBytes = newFeature((x: Byte) => x.toBytes, "{ (x: Byte) => x.toBytes }")
    lazy val toBits = newFeature((x: Byte) => x.toBits, "{ (x: Byte) => x.toBits }")
    lazy val toAbs = newFeature((x: Byte) => x.toAbs, "{ (x: Byte) => x.toAbs }")
    lazy val compareTo = newFeature(
      (x: (Byte, Byte)) => x._1.compareTo(x._2),
      "{ (x: (Byte, Byte)) => x._1.compareTo(x._2) }")

    val n = ExactNumeric.ByteIsExactNumeric
    lazy val arithOps = existingFeature(
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
    )

    lazy val bitOr = newFeature(
      { (x: (Byte, Byte)) => (x._1 | x._2).toByteExact },
      "{ (x: (Byte, Byte)) => (x._1 | x._2).toByteExact }")

    lazy val bitAnd = newFeature(
      { (x: (Byte, Byte)) => (x._1 & x._2).toByteExact },
      "{ (x: (Byte, Byte)) => (x._1 & x._2).toByteExact }")

    forAll { x: Byte =>
      Seq(toByte, toShort, toInt, toLong, toBigInt, toBytes, toBits, toAbs).foreach(f => f.checkEquality(x))
    }

    forAll { x: (Byte, Byte) =>
      Seq(compareTo, arithOps, bitOr, bitAnd).foreach(_.checkEquality(x))
    }

  }

  property("Short methods equivalence") {
    val toByte = existingFeature((x: Short) => x.toByteExact,
      "{ (x: Short) => x.toByte }",
      FuncValue(Vector((1, SShort)), Downcast(ValUse(1, SShort), SByte)))
    val toShort = existingFeature((x: Short) => x.toShort,
      "{ (x: Short) => x.toShort }",
      FuncValue(Vector((1, SShort)), ValUse(1, SShort)))
    val toInt = existingFeature((x: Short) => x.toInt,
      "{ (x: Short) => x.toInt }",
      FuncValue(Vector((1, SShort)), Upcast(ValUse(1, SShort), SInt)))
    val toLong = existingFeature((x: Short) => x.toLong,
      "{ (x: Short) => x.toLong }",
      FuncValue(Vector((1, SShort)), Upcast(ValUse(1, SShort), SLong)))
    val toBigInt = existingFeature((x: Short) => x.toBigInt,
      "{ (x: Short) => x.toBigInt }",
      FuncValue(Vector((1, SShort)), Upcast(ValUse(1, SShort), SBigInt)))

    lazy val toBytes = newFeature((x: Short) => x.toBytes, "{ (x: Short) => x.toBytes }")
    lazy val toBits = newFeature((x: Short) => x.toBits, "{ (x: Short) => x.toBits }")
    lazy val toAbs = newFeature((x: Short) => x.toAbs,
      "{ (x: Short) => x.toAbs }")
    lazy val compareTo = newFeature((x: (Short, Short)) => x._1.compareTo(x._2),
      "{ (x: (Short, Short)) => x._1.compareTo(x._2) }")

    val n = ExactNumeric.ShortIsExactNumeric
    lazy val arithOps = existingFeature(
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
    )

    lazy val bitOr = newFeature(
    { (x: (Short, Short)) => (x._1 | x._2).toShortExact },
    "{ (x: (Short, Short)) => x._1 | x._2 }")

    lazy val bitAnd = newFeature(
    { (x: (Short, Short)) => (x._1 & x._2).toShortExact },
    "{ (x: (Short, Short)) => x._1 & x._2 }")

    forAll { x: Short =>
      Seq(toByte, toShort, toInt, toLong, toBigInt, toBytes, toBits, toAbs).foreach(_.checkEquality(x))
    }
    forAll { x: (Short, Short) =>
      Seq(compareTo, arithOps, bitOr, bitAnd).foreach(_.checkEquality(x))
    }
  }

  property("Int methods equivalence") {
    val toByte = existingFeature((x: Int) => x.toByteExact,
      "{ (x: Int) => x.toByte }",
      FuncValue(Vector((1, SInt)), Downcast(ValUse(1, SInt), SByte)))
    val toShort = existingFeature((x: Int) => x.toShortExact,
      "{ (x: Int) => x.toShort }",
      FuncValue(Vector((1, SInt)), Downcast(ValUse(1, SInt), SShort)))
    val toInt = existingFeature((x: Int) => x.toInt,
      "{ (x: Int) => x.toInt }",
      FuncValue(Vector((1, SInt)), ValUse(1, SInt)))
    val toLong = existingFeature((x: Int) => x.toLong,
      "{ (x: Int) => x.toLong }",
      FuncValue(Vector((1, SInt)), Upcast(ValUse(1, SInt), SLong)))
    val toBigInt = existingFeature((x: Int) => x.toBigInt,
      "{ (x: Int) => x.toBigInt }",
      FuncValue(Vector((1, SInt)), Upcast(ValUse(1, SInt), SBigInt)))
    lazy val toBytes = newFeature((x: Int) => x.toBytes, "{ (x: Int) => x.toBytes }")
    lazy val toBits = newFeature((x: Int) => x.toBits, "{ (x: Int) => x.toBits }")
    lazy val toAbs = newFeature((x: Int) => x.toAbs, "{ (x: Int) => x.toAbs }")
    lazy val compareTo = newFeature((x: (Int, Int)) => x._1.compareTo(x._2),
      "{ (x: (Int, Int)) => x._1.compareTo(x._2) }")

    val n = ExactNumeric.IntIsExactNumeric
    lazy val arithOps = existingFeature(
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
        ))

    lazy val bitOr = newFeature(
    { (x: (Int, Int)) => x._1 | x._2 },
    "{ (x: (Int, Int)) => x._1 | x._2 }")

    lazy val bitAnd = newFeature(
    { (x: (Int, Int)) => x._1 & x._2 },
    "{ (x: (Int, Int)) => x._1 & x._2 }")

    forAll { x: Int =>
      Seq(toByte, toShort, toInt, toLong, toBigInt, toBytes, toBits, toAbs).foreach(_.checkEquality(x))
    }
    forAll { x: (Int, Int) =>
      Seq(compareTo, arithOps, bitOr, bitAnd).foreach(_.checkEquality(x))
    }
  }

  property("Long methods equivalence") {
    val toByte = existingFeature((x: Long) => x.toByteExact,
      "{ (x: Long) => x.toByte }",
      FuncValue(Vector((1, SLong)), Downcast(ValUse(1, SLong), SByte)))
    val toShort = existingFeature((x: Long) => x.toShortExact,
      "{ (x: Long) => x.toShort }",
      FuncValue(Vector((1, SLong)), Downcast(ValUse(1, SLong), SShort)))
    val toInt = existingFeature((x: Long) => x.toIntExact,
      "{ (x: Long) => x.toInt }",
      FuncValue(Vector((1, SLong)), Downcast(ValUse(1, SLong), SInt)))
    val toLong = existingFeature((x: Long) => x.toLong,
      "{ (x: Long) => x.toLong }",
      FuncValue(Vector((1, SLong)), ValUse(1, SLong)))
    val toBigInt = existingFeature((x: Long) => x.toBigInt,
      "{ (x: Long) => x.toBigInt }",
      FuncValue(Vector((1, SLong)), Upcast(ValUse(1, SLong), SBigInt)))
    lazy val toBytes = newFeature((x: Long) => x.toBytes, "{ (x: Long) => x.toBytes }")
    lazy val toBits = newFeature((x: Long) => x.toBits, "{ (x: Long) => x.toBits }")
    lazy val toAbs = newFeature((x: Long) => x.toAbs, "{ (x: Long) => x.toAbs }")
    lazy val compareTo = newFeature((x: (Long, Long)) => x._1.compareTo(x._2),
      "{ (x: (Long, Long)) => x._1.compareTo(x._2) }")

    val n = ExactNumeric.LongIsExactNumeric
    lazy val arithOps = existingFeature(
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
    ))

    lazy val bitOr = newFeature(
    { (x: (Long, Long)) => x._1 | x._2 },
    "{ (x: (Long, Long)) => x._1 | x._2 }")

    lazy val bitAnd = newFeature(
    { (x: (Long, Long)) => x._1 & x._2 },
    "{ (x: (Long, Long)) => x._1 & x._2 }")

    forAll { x: Long =>
      Seq(toByte, toShort, toInt, toLong, toBigInt, toBytes, toBits, toAbs).foreach(_.checkEquality(x))
    }
    forAll { x: (Long, Long) =>
      Seq(compareTo, arithOps, bitOr, bitAnd).foreach(_.checkEquality(x))
    }
  }

  property("BigInt methods equivalence") {
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

    val toBigInt = existingFeature((x: BigInt) => x,
      "{ (x: BigInt) => x.toBigInt }",
      FuncValue(Vector((1, SBigInt)), ValUse(1, SBigInt)))

    lazy val toBytes = newFeature((x: BigInt) => x.toBytes, "{ (x: BigInt) => x.toBytes }")
    lazy val toBits = newFeature((x: BigInt) => x.toBits, "{ (x: BigInt) => x.toBits }")
    lazy val toAbs = newFeature((x: BigInt) => x.toAbs, "{ (x: BigInt) => x.toAbs }")
    lazy val compareTo = newFeature((x: (BigInt, BigInt)) => x._1.compareTo(x._2),
      "{ (x: (BigInt, BigInt)) => x._1.compareTo(x._2) }")

    val n = NumericOps.BigIntIsExactNumeric
    lazy val arithOps = existingFeature(
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
    )

    lazy val bitOr = newFeature(
    { (x: (BigInt, BigInt)) => x._1 | x._2 },
    "{ (x: (BigInt, BigInt)) => x._1 | x._2 }")

    lazy val bitAnd = newFeature(
    { (x: (BigInt, BigInt)) => x._1 & x._2 },
    "{ (x: (BigInt, BigInt)) => x._1 & x._2 }")

    forAll { x: BigInt =>
      Seq(toByte, toShort, toInt, toLong, toBigInt, toBytes, toBits, toAbs).foreach(_.checkEquality(x))
    }
    forAll { x: (BigInt, BigInt) =>
      Seq(compareTo, arithOps, bitOr, bitAnd).foreach(_.checkEquality(x))
    }
  }

  property("GroupElement methods equivalence") {
    val getEncoded = existingFeature((x: GroupElement) => x.getEncoded,
      "{ (x: GroupElement) => x.getEncoded }",
      FuncValue(
        Vector((1, SGroupElement)),
        MethodCall(ValUse(1, SGroupElement), SGroupElement.getMethodByName("getEncoded"), Vector(), Map())
      ))

    val decode = existingFeature({ (x: GroupElement) => decodePoint(x.getEncoded) == x },
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
      ))

    val negate = existingFeature({ (x: GroupElement) => x.negate },
      "{ (x: GroupElement) => x.negate }",
      FuncValue(
        Vector((1, SGroupElement)),
        MethodCall(ValUse(1, SGroupElement), SGroupElement.getMethodByName("negate"), Vector(), Map())
      ))

    //TODO soft-fork: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
    // val isIdentity = existingFeature({ (x: GroupElement) => x.isIdentity },
    //   "{ (x: GroupElement) => x.isIdentity }")

    forAll { x: GroupElement =>
      Seq(getEncoded, decode, negate/*, isIdentity*/).foreach(_.checkEquality(x))
    }

    val exp = existingFeature({ (x: (GroupElement, BigInt)) => x._1.exp(x._2) },
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
      ))

    forAll { x: (GroupElement, BigInt) =>
      exp.checkEquality(x)
    }

    val multiply = existingFeature({ (x: (GroupElement, GroupElement)) => x._1.multiply(x._2) },
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
      ))

    forAll { x: (GroupElement, GroupElement) =>
      multiply.checkEquality(x)
    }
  }

  property("AvlTree properties equivalence") {
    val doDigest = checkEq(func[AvlTree, Coll[Byte]]("{ (t: AvlTree) => t.digest }")) { (t: AvlTree) => t.digest }
    val doEnabledOps = checkEq(func[AvlTree, Byte](
      "{ (t: AvlTree) => t.enabledOperations }")) { (t: AvlTree) => t.enabledOperations }
    val doKeyLength = checkEq(func[AvlTree, Int]("{ (t: AvlTree) => t.keyLength }")) { (t: AvlTree) => t.keyLength }
    val doValueLength = checkEq(func[AvlTree, Option[Int]]("{ (t: AvlTree) => t.valueLengthOpt }")) { (t: AvlTree) => t.valueLengthOpt }
    val insertAllowed = checkEq(func[AvlTree, Boolean]("{ (t: AvlTree) => t.isInsertAllowed }")) { (t: AvlTree) => t.isInsertAllowed }
    val updateAllowed = checkEq(func[AvlTree, Boolean]("{ (t: AvlTree) => t.isUpdateAllowed }")) { (t: AvlTree) => t.isUpdateAllowed }
    val removeAllowed = checkEq(func[AvlTree, Boolean]("{ (t: AvlTree) => t.isRemoveAllowed }")) { (t: AvlTree) => t.isRemoveAllowed }

    val newTree = sampleAvlTree.updateOperations(1.toByte)
    val trees = Array(sampleAvlTree, newTree)

    for (tree <- trees) {
      doDigest(tree)
      doEnabledOps(tree)
      doKeyLength(tree)
      doValueLength(tree)
      insertAllowed(tree)
      updateAllowed(tree)
      removeAllowed(tree)
    }
  }

  property("AvlTree.{contains, get, getMany, updateDigest, updateOperations} equivalence") {
    val doContains = checkEq(
      func[(AvlTree, (Coll[Byte], Coll[Byte])), Boolean](
      "{ (t: (AvlTree, (Coll[Byte], Coll[Byte]))) => t._1.contains(t._2._1, t._2._2) }"))
         { (t: (AvlTree, (Coll[Byte], Coll[Byte]))) => t._1.contains(t._2._1, t._2._2) }
    val doGet = checkEq(
      func[(AvlTree, (Coll[Byte], Coll[Byte])), Option[Coll[Byte]]](
      "{ (t: (AvlTree, (Coll[Byte], Coll[Byte]))) => t._1.get(t._2._1, t._2._2) }"))
         { (t: (AvlTree, (Coll[Byte], Coll[Byte]))) => t._1.get(t._2._1, t._2._2) }
    val doGetMany = checkEq(
      func[(AvlTree, (Coll[Coll[Byte]], Coll[Byte])), Coll[Option[Coll[Byte]]]](
      "{ (t: (AvlTree, (Coll[Coll[Byte]], Coll[Byte]))) => t._1.getMany(t._2._1, t._2._2) }"))
         { (t: (AvlTree, (Coll[Coll[Byte]], Coll[Byte]))) => t._1.getMany(t._2._1, t._2._2) }
    val doUpdateDigest = checkEq(
      func[(AvlTree, Coll[Byte]), AvlTree](
        "{ (t: (AvlTree, Coll[Byte])) => t._1.updateDigest(t._2) }"))
    { (t: (AvlTree, Coll[Byte])) => t._1.updateDigest(t._2) }
    val doUpdateOperations = checkEq(
      func[(AvlTree, Byte), AvlTree](
        "{ (t: (AvlTree, Byte)) => t._1.updateOperations(t._2) }"))
    { (t: (AvlTree, Byte)) => t._1.updateOperations(t._2) }

    val (key, _, avlProver) = sampleAvlProver
    avlProver.performOneOperation(Lookup(ADKey @@ key.toArray))
    val digest = avlProver.digest.toColl
    val proof = avlProver.generateProof().toColl
    val tree = SigmaDsl.avlTree(AvlTreeFlags.ReadOnly.serializeToByte, digest, 32, None)
    doContains((tree, (key, proof)))
    doGet((tree, (key, proof)))
    val keys = Colls.fromItems(key)
    doGetMany((tree, (keys, proof)))
    doUpdateDigest(tree, digest)
    doUpdateOperations(tree, 1.toByte)
  }

  property("AvlTree.{insert, update, remove} equivalence") {
    type KV = (Coll[Byte], Coll[Byte])
    val doInsert = checkEq(
      func[(AvlTree, (Coll[KV], Coll[Byte])), Option[AvlTree]](
      "{ (t: (AvlTree, (Coll[(Coll[Byte], Coll[Byte])], Coll[Byte]))) => t._1.insert(t._2._1, t._2._2) }"))
         { (t: (AvlTree, (Coll[KV], Coll[Byte]))) => t._1.insert(t._2._1, t._2._2) }
    val doUpdate = checkEq(
      func[(AvlTree, (Coll[KV], Coll[Byte])), Option[AvlTree]](
        "{ (t: (AvlTree, (Coll[(Coll[Byte], Coll[Byte])], Coll[Byte]))) => t._1.update(t._2._1, t._2._2) }"))
        { (t: (AvlTree, (Coll[KV], Coll[Byte]))) => t._1.update(t._2._1, t._2._2) }
    val doRemove = checkEq(
      func[(AvlTree, (Coll[Coll[Byte]], Coll[Byte])), Option[AvlTree]](
      "{ (t: (AvlTree, (Coll[Coll[Byte]], Coll[Byte]))) => t._1.remove(t._2._1, t._2._2) }"))
         { (t: (AvlTree, (Coll[Coll[Byte]], Coll[Byte]))) => t._1.remove(t._2._1, t._2._2) }

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    val key = Array.fill(32)(1.toByte).toColl

    {
      val preInsertDigest = avlProver.digest.toColl
      val value = bytesCollGen.sample.get
      avlProver.performOneOperation(Insert(ADKey @@ key.toArray, ADValue @@ value.toArray))
      val insertProof = avlProver.generateProof().toColl
      val preInsertTree = SigmaDsl.avlTree(AvlTreeFlags(true, false, false).serializeToByte, preInsertDigest, 32, None)
      val insertKvs = Colls.fromItems((key -> value))
      doInsert((preInsertTree, (insertKvs, insertProof)))
    }

    {
      val preUpdateDigest = avlProver.digest.toColl
      val newValue = bytesCollGen.sample.get
      avlProver.performOneOperation(Update(ADKey @@ key.toArray, ADValue @@ newValue.toArray))
      val updateProof = avlProver.generateProof().toColl
      val preUpdateTree = SigmaDsl.avlTree(AvlTreeFlags(false, true, false).serializeToByte, preUpdateDigest, 32, None)
      val updateKvs = Colls.fromItems((key -> newValue))
      doUpdate((preUpdateTree, (updateKvs, updateProof)))
    }

    {
      val preRemoveDigest = avlProver.digest.toColl
      avlProver.performOneOperation(Remove(ADKey @@ key.toArray))
      val removeProof = avlProver.generateProof().toColl
      val preRemoveTree = SigmaDsl.avlTree(AvlTreeFlags(false, false, true).serializeToByte, preRemoveDigest, 32, None)
      val removeKeys = Colls.fromItems(key)
      doRemove((preRemoveTree, (removeKeys, removeProof)))
    }
  }

  property("longToByteArray equivalence") {
    val eq = checkEq(func[Long, Coll[Byte]]("{ (x: Long) => longToByteArray(x) }")){ x =>
      longToByteArray(x)
    }
    forAll { x: Long => eq(x) }
  }

  property("byteArrayToBigInt equivalence") {
    val eq = checkEq(func[Coll[Byte], BigInt]("{ (x: Coll[Byte]) => byteArrayToBigInt(x) }")){ x =>
      byteArrayToBigInt(x)
    }
    forAll { x: Array[Byte] =>
      whenever(x.length <= SigmaConstants.MaxBigIntSizeInBytes.value) {
        eq(Colls.fromArray(x))
      }
    }
  }

  property("byteArrayToLong equivalence") {
    val eq = checkEq(func[Coll[Byte],Long]("{ (x: Coll[Byte]) => byteArrayToLong(x) }")){ x =>
      byteArrayToLong(x)
    }
    forAll { x: Array[Byte] =>
      whenever(x.length >= 8) {
        eq(Colls.fromArray(x))
      }
    }
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
    val box = ctx.dataInputs(0)
    val eq = EqualityChecker(box)
    eq({ (x: Box) => x.id })("{ (x: Box) => x.id }")
    eq({ (x: Box) => x.value })("{ (x: Box) => x.value }")
    eq({ (x: Box) => x.propositionBytes })("{ (x: Box) => x.propositionBytes }")
    eq({ (x: Box) => x.bytes })("{ (x: Box) => x.bytes }")
    eq({ (x: Box) => x.bytesWithoutRef })("{ (x: Box) => x.bytesWithoutRef }")
    eq({ (x: Box) => x.creationInfo })("{ (x: Box) => x.creationInfo }")
    eq({ (x: Box) => x.tokens })("{ (x: Box) => x.tokens }")
  }

  property("Advanced Box test") {
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    avlProver.generateProof()

    val digest = avlProver.digest
    val treeData = SigmaDsl.avlTree(new AvlTreeData(digest, AvlTreeFlags.ReadOnly, 32, None))
    val box = ctx.dataInputs(0)

    val s = ErgoBox(20, TrueProp, 0, Seq(),Map(
      ErgoBox.nonMandatoryRegisters(0) -> ByteConstant(1.toByte),
      ErgoBox.nonMandatoryRegisters(1) -> ShortConstant(1024.toShort),
      ErgoBox.nonMandatoryRegisters(2) -> IntConstant(1024 * 1024),
      ErgoBox.nonMandatoryRegisters(3) -> LongConstant(1024.toLong),
      ErgoBox.nonMandatoryRegisters(4) -> BigIntConstant(222L),
      ErgoBox.nonMandatoryRegisters(5) -> AvlTreeConstant(treeData)
    ))
    lazy val byteCheck = checkEq(func[Box,Byte]("{ (x: Box) => x.R4[Byte].get }"))((x: Box) => x.R4[Byte].get)
    lazy val shortCheck = checkEq(func[Box,Short]("{ (x: Box) => x.R5[Short].get }"))((x: Box) => x.R5[Short].get)
    lazy val intCheck = checkEq(func[Box,Int]("{ (x: Box) => x.R6[Int].get }"))((x: Box) => x.R6[Int].get)
    lazy val longCheck = checkEq(func[Box,Long]("{ (x: Box) => x.R7[Long].get }"))((x: Box) => x.R7[Long].get)
    lazy val BigIntCheck = checkEq(func[Box,BigInt]("{ (x: Box) => x.R8[BigInt].get }"))((x: Box) => x.R8[BigInt].get)
    byteCheck(s)
    shortCheck(s)
    intCheck(s)
    longCheck(s)
    BigIntCheck(s)
  }

  property("PreHeader properties equivalence") {
    val h = ctx.preHeader
    val eq = EqualityChecker(h)
    eq({ (x: PreHeader) => x.version })("{ (x: PreHeader) => x.version }")
    eq({ (x: PreHeader) => x.parentId })("{ (x: PreHeader) => x.parentId }")
    eq({ (x: PreHeader) => x.timestamp })("{ (x: PreHeader) => x.timestamp }")
    eq({ (x: PreHeader) => x.nBits })("{ (x: PreHeader) => x.nBits }")
    eq({ (x: PreHeader) => x.height })("{ (x: PreHeader) => x.height }")
    eq({ (x: PreHeader) => x.minerPk })("{ (x: PreHeader) => x.minerPk }")
    eq({ (x: PreHeader) => x.votes })("{ (x: PreHeader) => x.votes }")
  }

  property("Header properties equivalence") {
    val h = ctx.headers(0)
    val eq = EqualityChecker(h)
    eq({ (x: Header) => x.id })("{ (x: Header) => x.id }")
    eq({ (x: Header) => x.version })("{ (x: Header) => x.version }")
    eq({ (x: Header) => x.parentId })("{ (x: Header) => x.parentId }")
    eq({ (x: Header) => x.ADProofsRoot})("{ (x: Header) => x.ADProofsRoot}")
    eq({ (x: Header) => x.stateRoot })("{ (x: Header) => x.stateRoot }")
    eq({ (x: Header) => x.transactionsRoot })("{ (x: Header) => x.transactionsRoot }")
    eq({ (x: Header) => x.timestamp })("{ (x: Header) => x.timestamp }")
    eq({ (x: Header) => x.nBits })("{ (x: Header) => x.nBits }")
    eq({ (x: Header) => x.height })("{ (x: Header) => x.height }")
    eq({ (x: Header) => x.extensionRoot })("{ (x: Header) => x.extensionRoot }")
    eq({ (x: Header) => x.minerPk })("{ (x: Header) => x.minerPk }")
    eq({ (x: Header) => x.powOnetimePk })("{ (x: Header) => x.powOnetimePk }")
    eq({ (x: Header) => x.powNonce })("{ (x: Header) => x.powNonce }")
    eq({ (x: Header) => x.powDistance })("{ (x: Header) => x.powDistance }")
    eq({ (x: Header) => x.votes })("{ (x: Header) => x.votes }")
  }

  property("Context properties equivalence") {
    val eq = EqualityChecker(ctx)
    eq({ (x: Context) => x.dataInputs })("{ (x: Context) => x.dataInputs }")
    eq({ (x: Context) => x.dataInputs(0) })("{ (x: Context) => x.dataInputs(0) }")
    eq({ (x: Context) => x.dataInputs(0).id })("{ (x: Context) => x.dataInputs(0).id }")
    eq({ (x: Context) => x.preHeader })("{ (x: Context) => x.preHeader }")
    eq({ (x: Context) => x.headers })("{ (x: Context) => x.headers }")

    eq({ (x: Context) => x.OUTPUTS })("{ (x: Context) => x.OUTPUTS }")

    eq({ (x: Context) => x.INPUTS })("{ (x: Context) => x.INPUTS }")
    eq({ (x: Context) => x.HEIGHT })("{ (x: Context) => x.HEIGHT }")
    eq({ (x: Context) => x.SELF })("{ (x: Context) => x.SELF }")
    eq({ (x: Context) => x.INPUTS.map { (b: Box) => b.value } })("{ (x: Context) => x.INPUTS.map { (b: Box) => b.value } }")

    eq({ (x: Context) => x.selfBoxIndex })("{ (x: Context) => x.selfBoxIndex }")
    eq({ (x: Context) => x.LastBlockUtxoRootHash.isUpdateAllowed })("{ (x: Context) => x.LastBlockUtxoRootHash.isUpdateAllowed }")
    eq({ (x: Context) => x.minerPubKey })("{ (x: Context) => x.minerPubKey }")

    eq({ (x: Context) =>
      x.INPUTS.map { (b: Box) => (b.value, b.value) }
    })(
      """{ (x: Context) =>
       |  x.INPUTS.map { (b: Box) => (b.value, b.value) }
       |}""".stripMargin
    )

    eq({ (x: Context) =>
      x.INPUTS.map { (b: Box) =>
        val pk = b.R4[Int].get
        val value = longToByteArray(b.value)
        (pk, value)
      }
    })(
    """{ (x: Context) =>
     |  x.INPUTS.map { (b: Box) =>
     |    val pk = b.R4[Int].get
     |    val value = longToByteArray(b.value)
     |    (pk, value)
     |  }
     |}""".stripMargin)
  }

  property("getVar equivalence") {
    val eq = checkEq(func[Int,Int]("{ (x: Int) => getVar[Int](2).get }", 2.toByte -> IntConstant(10))) { x =>
      10
    }
    eq(1)
  }

  property("xorOf equivalence") {
    val eq = checkEq(func[Coll[Boolean], Boolean]("{ (x: Coll[Boolean]) => xorOf(x) }")) { x =>
      xorOf(x)
    }
    forAll { x: Array[Boolean] =>
      eq(Colls.fromArray(x))
    }
  }

  property("LogicalNot equivalence") {
    // TODO make a prefix method
    val eq = checkEq(func[Boolean, Boolean]("{ (x: Boolean) => !x }")) { x => !x }
    forAll { x: Boolean => eq(x) }
  }

  property("Negation equivalence") {
    // TODO make a prefix method
    val negByte = checkEq(func[Byte, Byte]("{ (x: Byte) => -x }")) { (x: Byte) => (-x).toByte }
    forAll { x: Byte => negByte(x) }
    val negShort = checkEq(func[Short, Short]("{ (x: Short) => -x }")) { (x: Short) => (-x).toShort }
    forAll { x: Short => negShort(x) }
    val negInt = checkEq(func[Int, Int]("{ (x: Int) => -x }")) { (x: Int) => -x }
    forAll { x: Int => negInt(x) }
    val negLong = checkEq(func[Long, Long]("{ (x: Long) => -x }")) { (x: Long) => -x }
    forAll { x: Long => negLong(x) }
  }

  property("special.sigma.BigInt Negation equivalence") {
    // TODO make negate() into a prefix method
    val negBigInteger = checkEq(func[BigInt, BigInt]("{ (x: BigInt) => -x }")) { (x: BigInt) => x.negate() }
    forAll { x: BigInt => negBigInteger(x) }
  }

  // TODO: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/416
  ignore("Box.getReg equivalence") {
//    val eq = checkEq(func[Box, Int]("{ (x: Box) => x.getReg[Int](1).get }")) { x => x.getReg(1).get }
//    forAll { x: Box => eq(x) }
  }

  property("global functions equivalence") {
    val n = SigmaDsl.BigInt(BigInteger.TEN)
    val Global = SigmaDsl

    {
      val eq = EqualityChecker(1)
      eq({ (x: Int) => groupGenerator })("{ (x: Int) => groupGenerator }")
      eq({ (x: Int) => Global.groupGenerator })("{ (x: Int) => Global.groupGenerator }")
    }

    {
      val eq = EqualityChecker(n)
      eq({ (n: BigInt) => groupGenerator.exp(n) })("{ (n: BigInt) => groupGenerator.exp(n) }")
    }

    {
      val eq = checkEq(func[(Coll[Byte], Coll[Byte]), Coll[Byte]](
        "{ (x: (Coll[Byte], Coll[Byte])) => xor(x._1, x._2) }"))
        { x => Global.xor(x._1, x._2) }
      forAll(bytesGen, bytesGen) { (l, r) =>
        eq(Colls.fromArray(l), Colls.fromArray(r))
      }
    }
  }

  property("Coll methods equivalence") {
    val coll = ctx.OUTPUTS
    val eq = EqualityChecker(coll)
    eq({ (x: Coll[Box]) => x.filter({ (b: Box) => b.value > 1 }) })("{ (x: Coll[Box]) => x.filter({(b: Box) => b.value > 1 }) }")
    eq({ (x: Coll[Box]) => x.flatMap({ (b: Box) => b.propositionBytes }) })("{ (x: Coll[Box]) => x.flatMap({(b: Box) => b.propositionBytes }) }")
    eq({ (x: Coll[Box]) => x.zip(x) })("{ (x: Coll[Box]) => x.zip(x) }")
    eq({ (x: Coll[Box]) => x.size })("{ (x: Coll[Box]) => x.size }")
    eq({ (x: Coll[Box]) => x.indices })("{ (x: Coll[Box]) => x.indices }")
    eq({ (x: Coll[Box]) => x.forall({ (b: Box) => b.value > 1 }) })("{ (x: Coll[Box]) => x.forall({(b: Box) => b.value > 1 }) }")
    eq({ (x: Coll[Box]) => x.exists({ (b: Box) => b.value > 1 }) })("{ (x: Coll[Box]) => x.exists({(b: Box) => b.value > 1 }) }")
  }

  property("Coll size method equivalence") {
    val eq = checkEq(func[Coll[Int],Int]("{ (x: Coll[Int]) => x.size }")){ x =>
      x.size
    }
    forAll { x: Array[Int] =>
      eq(Colls.fromArray(x))
    }
  }

  val arrayWithRangeGen = for {
    arr <- arrayGen[Int];
    l <- Gen.choose(0, arr.length - 1);
    r <- Gen.choose(l, arr.length - 1) } yield (arr, (l, r))

  property("Coll patch method equivalence") {
    val eq = checkEq(func[(Coll[Int], (Int, Int)),Coll[Int]]("{ (x: (Coll[Int], (Int, Int))) => x._1.patch(x._2._1, x._1, x._2._2) }")){ x =>
      x._1.patch(x._2._1, x._1, x._2._2)
    }
    forAll(arrayWithRangeGen) { data =>
      val arr = data._1
      val range = data._2
      whenever (arr.length > 1) {
        eq(Colls.fromArray(arr), range)
      }
    }
  }

  property("Coll updated method equivalence") {
    val eq = checkEq(func[(Coll[Int], (Int, Int)),Coll[Int]]("{ (x: (Coll[Int], (Int, Int))) => x._1.updated(x._2._1, x._2._2) }")){ x =>
      x._1.updated(x._2._1, x._2._2)
    }
    forAll { x: (Array[Int], Int) =>
      val size = x._1.size
      whenever (size > 1) {
        val index = getArrayIndex(size)
        eq(Colls.fromArray(x._1), (index, x._2))
      }
    }
  }

  property("Coll updateMany method equivalence") {
    val eq = checkEq(func[(Coll[Int], (Coll[Int], Coll[Int])),Coll[Int]](
      "{ (x: (Coll[Int], (Coll[Int], Coll[Int]))) => x._1.updateMany(x._2._1, x._2._2) }"))
      { x => x._1.updateMany(x._2._1, x._2._2) }

    val dataGen = for {
      arr <- arrayGen[Int];
      len <- Gen.choose(0, arr.length)
      indices <- Gen.containerOfN[Array, Int](len, Gen.choose(0, arr.length - 1))
      } yield (arr, indices)

    forAll(dataGen) { data =>
      val arr = data._1
      val indices = data._2
      whenever (arr.length > 1) {
        val xs = Colls.fromArray(arr)
        val is = Colls.fromArray(indices)
        val vs = is.reverse
        val input = (xs, (is, vs))
        eq(input)
      }
    }
  }

  // TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
  ignore("Coll find method equivalence") {
    val eq = checkEq(func[Coll[Int],Option[Int]]("{ (x: Coll[Int]) => x.find({(v: Int) => v > 0})}")){ x =>
      x.find(v => v > 0)
    }
    forAll { x: Array[Int] =>
      eq(Colls.fromArray(x))
    }
  }

  // https://github.com/ScorexFoundation/sigmastate-interpreter/issues/418
  ignore("Coll bitwise methods equivalence") {
    val eq = checkEq(func[Coll[Boolean],Coll[Boolean]]("{ (x: Coll[Boolean]) => x >> 2 }")){ x =>
      if (x.size > 2) x.slice(0, x.size - 2) else Colls.emptyColl
    }
    forAll { x: Array[Boolean] =>
      eq(Colls.fromArray(x))
    }
  }

  // TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
  ignore("Coll diff methods equivalence") {
    val eq = checkEq(func[Coll[Int],Coll[Int]]("{ (x: Coll[Int]) => x.diff(x) }")){ x =>
      x.diff(x)
    }
    forAll { x: Array[Int] =>
      eq(Colls.fromArray(x))
    }
  }

  property("Coll fold method equivalence") {
    val eq = checkEq(func[(Coll[Byte], Int),Int]("{ (x: (Coll[Byte], Int)) => x._1.fold(x._2, { (i1: Int, i2: Byte) => i1 + i2 }) }"))
    { x =>
      x._1.foldLeft(x._2, { i: (Int, Byte) => i._1 + i._2 })
    }
    val eqIndexOf = checkEq(func[(Coll[Byte], Byte),Int]("{ (x: (Coll[Byte], Byte)) => x._1.indexOf(x._2, 0) }"))
    { x =>
      x._1.indexOf(x._2, 0)
    }
    forAll { x: (Array[Byte], Short, Byte) =>
      eq(Colls.fromArray(x._1), x._2)
      eqIndexOf(Colls.fromArray(x._1), x._3)
    }
  }

  property("Coll indexOf method equivalence") {
    val eqIndexOf = checkEq(func[(Coll[Int], (Int, Int)),Int]("{ (x: (Coll[Int], (Int, Int))) => x._1.indexOf(x._2._1, x._2._2) }"))
    { x =>
      x._1.indexOf(x._2._1, x._2._2)
    }
    forAll { x: (Array[Int], Int) =>
      eqIndexOf(Colls.fromArray(x._1), (getArrayIndex(x._1.size), x._2))
    }
  }

  property("Coll apply method equivalence") {
    val eqApply = checkEq(func[(Coll[Int], Int),Int]("{ (x: (Coll[Int], Int)) => x._1(x._2) }"))
    { x =>
      x._1(x._2)
    }
    forAll { x: Array[Int] =>
      whenever (0 < x.size) {
        eqApply(Colls.fromArray(x), getArrayIndex(x.size))
      }
    }
  }

  property("Coll getOrElse method equivalence") {
    val eqGetOrElse = checkEq(func[(Coll[Int], (Int, Int)),Int]("{ (x: (Coll[Int], (Int, Int))) => x._1.getOrElse(x._2._1, x._2._2) }"))
    { x =>
      x._1.getOrElse(x._2._1, x._2._2)
    }
    forAll { x: (Array[Int], (Int, Int)) =>
      eqGetOrElse(Colls.fromArray(x._1), x._2)
    }
  }

  property("Tuple size method equivalence") {
    val eq = checkEq(func[(Int, Int),Int]("{ (x: (Int, Int)) => x.size }")) { x => 2 }
    eq((-1, 1))
  }

  property("Tuple apply method equivalence") {
    val eq1 = checkEq(func[(Int, Int),Int]("{ (x: (Int, Int)) => x(0) }")) { x => -1 }
    val eq2 = checkEq(func[(Int, Int),Int]("{ (x: (Int, Int)) => x(1) }")) { x => 1 }
    eq1((-1, 1))
    eq2((-1, 1))
  }

  property("Coll map method equivalence") {
    val eq = checkEq(func[Coll[Int],Coll[Int]]("{ (x: Coll[Int]) => x.map({ (v: Int) => v + 1 }) }"))
    { x =>
      x.map(v => v + 1)
    }
    forAll { x: Array[Int] =>
      eq(Colls.fromArray(x.filter(_ < Int.MaxValue)))
    }
  }

  property("Coll slice method equivalence") {
    val eq = checkEq(func[(Coll[Int], (Int, Int)),Coll[Int]]("{ (x: (Coll[Int], (Int, Int))) => x._1.slice(x._2._1, x._2._2) }"))
    { x =>
      x._1.slice(x._2._1, x._2._2)
    }
    forAll(arrayWithRangeGen) { data =>
      val arr = data._1
      val range = data._2
      whenever (arr.length > 0) {
        val input = (Colls.fromArray(arr), range)
        eq(input)
      }
    }
    val arr = Array[Int](1, 2, 3, 4, 5)
    eq(Colls.fromArray(arr), (0, 2))
  }

  property("Coll append method equivalence") {
    val eq = checkEq(func[(Coll[Int], (Int, Int)),Coll[Int]](
      """{ (x: (Coll[Int], (Int, Int))) =>
        |val sliced: Coll[Int] = x._1.slice(x._2._1, x._2._2)
        |val toAppend: Coll[Int] = x._1
        |sliced.append(toAppend)
        |}""".stripMargin))
    { x =>
      val sliced: Coll[Int] = x._1.slice(x._2._1, x._2._2)
      val toAppend: Coll[Int] = x._1
      sliced.append(toAppend)
    }

    forAll(arrayWithRangeGen) { data =>
      val arr = data._1
      val range = data._2
      whenever (arr.length > 0) {
        val input = (Colls.fromArray(arr), range)
        eq(input)
      }
    }
  }

  property("Option methods equivalence") {
    val opt: Option[Long] = ctx.dataInputs(0).R0[Long]
    val eq = EqualityChecker(opt)
    eq({ (x: Option[Long]) => x.get })("{ (x: Option[Long]) => x.get }")
    // TODO implement Option.isEmpty
    //  eq({ (x: Option[Long]) => x.isEmpty })("{ (x: Option[Long]) => x.isEmpty }")
    eq({ (x: Option[Long]) => x.isDefined })("{ (x: Option[Long]) => x.isDefined }")
    eq({ (x: Option[Long]) => x.getOrElse(1L) })("{ (x: Option[Long]) => x.getOrElse(1L) }")
    eq({ (x: Option[Long]) => x.filter({ (v: Long) => v == 1} ) })("{ (x: Option[Long]) => x.filter({ (v: Long) => v == 1 }) }")
    eq({ (x: Option[Long]) => x.map( (v: Long) => v + 1 ) })("{ (x: Option[Long]) => x.map({ (v: Long) => v + 1 }) }")
  }

  // TODO implement Option.fold
  ignore("Option fold method") {
    val opt: Option[Long] = ctx.dataInputs(0).R0[Long]
    val eq = EqualityChecker(opt)
    eq({ (x: Option[Long]) => x.fold(5.toLong)( (v: Long) => v + 1 ) })("{ (x: Option[Long]) => x.fold(5, { (v: Long) => v + 1 }) }")
  }

  property("Option fold workaround method") {
    val opt: Option[Long] = ctx.dataInputs(0).R0[Long]
    val eq = EqualityChecker(opt)
    eq({ (x: Option[Long]) => x.fold(5.toLong)( (v: Long) => v + 1 ) })(
      """{(x: Option[Long]) =>
        |  def f(opt: Long): Long = opt + 1
        |  if (x.isDefined) f(x.get) else 5L
        |}""".stripMargin)
  }

  property("blake2b256, sha256 equivalence") {
    val eqBlake2b256 = checkEq(func[Coll[Byte], Coll[Byte]]("{ (x: Coll[Byte]) => blake2b256(x) }")){ x =>
      blake2b256(x)
    }
    val eqSha256 = checkEq(func[Coll[Byte], Coll[Byte]]("{ (x: Coll[Byte]) => sha256(x) }")){ x =>
      sha256(x)
    }
    forAll { x: Array[Byte] =>
      Seq(eqBlake2b256, eqSha256).foreach(_(Colls.fromArray(x)))
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
    lazy val eq = existingFeature((x: Coll[SigmaProp]) => atLeast(x.size - 1, x),
      "{ (x: Coll[SigmaProp]) => atLeast(x.size - 1, x) }",
      FuncValue(
        Vector((1, SCollectionType(SSigmaProp))),
        AtLeast(
          ArithOp(SizeOf(ValUse(1, SCollectionType(SSigmaProp))), IntConstant(1), OpCode @@ (-103.toByte)),
          ValUse(1, SCollectionType(SSigmaProp))
        )
      ))
    forAll(arrayGen[SigmaProp].suchThat(_.length > 2)) { x: Array[SigmaProp] =>
      eq.checkEquality(Colls.fromArray(x))
    }
  }

  property("&& sigma equivalence") {
    lazy val SigmaAnd1 = existingFeature(
      (x: (SigmaProp, SigmaProp)) => x._1 && x._2,
      "{ (x:(SigmaProp, SigmaProp)) => x._1 && x._2 }",
      FuncValue(
        Vector((1, STuple(Vector(SSigmaProp, SSigmaProp)))),
        SigmaAnd(
          Seq(
            SelectField.typed[SigmaPropValue](ValUse(1, STuple(Vector(SSigmaProp, SSigmaProp))), 1.toByte),
            SelectField.typed[SigmaPropValue](ValUse(1, STuple(Vector(SSigmaProp, SSigmaProp))), 2.toByte)
          )
        )
      ))
    lazy val SigmaAnd2 = existingFeature(
      (x: (SigmaProp, Boolean)) => x._1 && sigmaProp(x._2),
      "{ (x:(SigmaProp, Boolean)) => x._1 && sigmaProp(x._2) }",
      FuncValue(
        Vector((1, STuple(Vector(SSigmaProp, SBoolean)))),
        SigmaAnd(
          Seq(
            SelectField.typed[SigmaPropValue](ValUse(1, STuple(Vector(SSigmaProp, SBoolean))), 1.toByte),
            BoolToSigmaProp(
              SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SSigmaProp, SBoolean))), 2.toByte)
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
        Vector((1, STuple(Vector(SSigmaProp, SSigmaProp)))),
        SigmaOr(
          Seq(
            SelectField.typed[SigmaPropValue](ValUse(1, STuple(Vector(SSigmaProp, SSigmaProp))), 1.toByte),
            SelectField.typed[SigmaPropValue](ValUse(1, STuple(Vector(SSigmaProp, SSigmaProp))), 2.toByte)
          )
        )
      ))
    lazy val SigmaOr2 = existingFeature(
      (x: (SigmaProp, Boolean)) => x._1 || sigmaProp(x._2),
      "{ (x:(SigmaProp, Boolean)) => x._1 || sigmaProp(x._2) }",
      FuncValue(
        Vector((1, STuple(Vector(SSigmaProp, SBoolean)))),
        SigmaOr(
          Seq(
            SelectField.typed[SigmaPropValue](ValUse(1, STuple(Vector(SSigmaProp, SBoolean))), 1.toByte),
            BoolToSigmaProp(
              SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SSigmaProp, SBoolean))), 2.toByte)
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
    lazy val eq = checkEq(func[SigmaProp, Coll[Byte]]("{ (x: SigmaProp) => x.propBytes }")){ (x: SigmaProp) =>
      x.propBytes
    }
    forAll { x: SigmaProp =>
      eq(x)
    }
  }

  // TODO: implement allZK func https://github.com/ScorexFoundation/sigmastate-interpreter/issues/543
  ignore("allZK equivalence") {
    lazy val eq = checkEq(func[Coll[SigmaProp], SigmaProp]("{ (x: Coll[SigmaProp]) => allZK(x) }")){ (x: Coll[SigmaProp]) =>
      allZK(x)
    }
    forAll(arrayGen[SigmaProp].suchThat(_.length > 2)) { x: Array[SigmaProp] =>
      eq(Colls.fromArray(x))
    }
  }

  // TODO: implement anyZK func https://github.com/ScorexFoundation/sigmastate-interpreter/issues/543
  ignore("anyZK equivalence") {
    lazy val eq = checkEq(func[Coll[SigmaProp], SigmaProp]("{ (x: Coll[SigmaProp]) => anyZK(x) }")){ (x: Coll[SigmaProp]) =>
      anyZK(x)
    }
    forAll(arrayGen[SigmaProp].suchThat(_.length > 2)) { x: Array[SigmaProp] =>
      eq(Colls.fromArray(x))
    }
  }

  property("allOf equivalence") {
    lazy val eq = checkEq(func[Coll[Boolean], Boolean]("{ (x: Coll[Boolean]) => allOf(x) }")){ (x: Coll[Boolean]) =>
      allOf(x)
    }
    forAll(arrayGen[Boolean].suchThat(_.length > 2)) { x: Array[Boolean] =>
      eq(Colls.fromArray(x))
    }
  }

  property("anyOf equivalence") {
    lazy val eq = checkEq(func[Coll[Boolean], Boolean]("{ (x: Coll[Boolean]) => anyOf(x) }")){ (x: Coll[Boolean]) =>
      anyOf(x)
    }
    forAll(arrayGen[Boolean].suchThat(_.length > 2)) { x: Array[Boolean] =>
      eq(Colls.fromArray(x))
    }
  }

  property("proveDlog equivalence") {
    val eq = EqualityChecker(SigmaDsl.groupGenerator)
    eq({ (x: GroupElement) => proveDlog(x) })("{ (x: GroupElement) => proveDlog(x) }")
  }

  property("proveDHTuple equivalence") {
    val eq = EqualityChecker(SigmaDsl.groupGenerator)
    eq({ (x: GroupElement) => proveDHTuple(x, x, x, x) })("{ (x: GroupElement) => proveDHTuple(x, x, x, x) }")
  }

}
