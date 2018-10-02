package sigmastate

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scapi.sigma.DLogProtocol.{DLogProverInput, ProveDlog}
import scorex.crypto.hash.Blake2b256
import sigmastate.Values._
import sigmastate.interpreter._
import sigmastate.lang.{SigmaCompiler, TransformingSigmaBuilder}
import sigmastate.utxo.CostTable
import sigmastate.lang.Terms._
import org.ergoplatform.{ErgoBox, Height}
import sigmastate.eval.Evaluation
import special.sigma
import org.ergoplatform.{Height, ErgoBox}
import scorex.util.encode.Base58
import sigmastate.serialization.ValueSerializer

import scala.util.Random



class TestingInterpreterSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  import TestingInterpreter._

  implicit val soundness = CryptoConstants.soundnessBits

  property("Reduction to crypto #1") {
    forAll() { (h: Int) =>
      whenever(h > 0 && h < Int.MaxValue - 1) {
        val dk1 = DLogProverInput.random().publicImage

        val ctx = TestingContext(h)
        assert(reduceToCrypto(ctx, AND(GE(Height, LongConstant(h - 1)), dk1)).get._1.isInstanceOf[ProveDlog])
        assert(reduceToCrypto(ctx, AND(GE(Height, LongConstant(h)), dk1)).get._1.isInstanceOf[ProveDlog])
        assert(reduceToCrypto(ctx, AND(GE(Height, LongConstant(h + 1)), dk1)).get._1.isInstanceOf[FalseLeaf.type])

        assert(reduceToCrypto(ctx, OR(GE(Height, LongConstant(h - 1)), dk1)).get._1.isInstanceOf[TrueLeaf.type])
        assert(reduceToCrypto(ctx, OR(GE(Height, LongConstant(h)), dk1)).get._1.isInstanceOf[TrueLeaf.type])
        assert(reduceToCrypto(ctx, OR(GE(Height, LongConstant(h + 1)), dk1)).get._1.isInstanceOf[ProveDlog])
      }
    }
  }

  property("Reduction to crypto #2") {
    forAll() { (h: Int) =>

      whenever(h > 0 && h < Int.MaxValue - 1) {

        val dk1 = DLogProverInput.random().publicImage
        val dk2 = DLogProverInput.random().publicImage

        val ctx = TestingContext(h)

        assert(reduceToCrypto(ctx, OR(
                  AND(LE(Height, LongConstant(h + 1)), AND(dk1, dk2)),
                  AND(GT(Height, LongConstant(h + 1)), dk1)
                )).get._1.isInstanceOf[CAND])


        assert(reduceToCrypto(ctx, OR(
                  AND(LE(Height, LongConstant(h - 1)), AND(dk1, dk2)),
                  AND(GT(Height, LongConstant(h - 1)), dk1)
                )).get._1.isInstanceOf[ProveDlog])


        assert(reduceToCrypto(ctx, OR(
                  AND(LE(Height, LongConstant(h - 1)), AND(dk1, dk2)),
                  AND(GT(Height, LongConstant(h + 1)), dk1)
                )).get._1.isInstanceOf[FalseLeaf.type])

        assert(reduceToCrypto(ctx, OR(OR(
                  AND(LE(Height, LongConstant(h - 1)), AND(dk1, dk2)),
                  AND(GT(Height, LongConstant(h + 1)), dk1)
                ), AND(GT(Height, LongConstant(h - 1)), LE(Height, LongConstant(h + 1))))).get._1.isInstanceOf[TrueLeaf.type])

      }
    }
  }

  val compiler = new SigmaCompiler(TransformingSigmaBuilder)
  def compile(env: Map[String, Any], code: String): Value[SType] = {
    compiler.compile(env, code)
  }

  def testEval(code: String) = {
    val reg1 = ErgoBox.nonMandatoryRegisters.head
    val reg2 = ErgoBox.nonMandatoryRegisters(1)

    val dk1 = ProveDlog(secrets(0).publicImage.h)
    val dk2 = ProveDlog(secrets(1).publicImage.h)
    val ctx = TestingContext(99)
    val env = Map(
      "dk1" -> dk1,
      "dk2" -> dk2,
      "bytes1" -> Array[Byte](1, 2, 3),
      "bytes2" -> Array[Byte](4, 5, 6),
      "box1" -> ErgoBox(10, TrueLeaf, Seq(), Map(
          reg1 -> IntArrayConstant(Array[Int](1, 2, 3)),
          reg2 -> BoolArrayConstant(Array[Boolean](true, false, true)))))
    val prop = compile(env, code).asBoolValue
    println(code)
    println(prop)
    val challenge = Array.fill(32)(Random.nextInt(100).toByte)
    val proof1 = TestingInterpreter.prove(prop, ctx, challenge).get.proof
    verify(prop, ctx, proof1, challenge).map(_._1).getOrElse(false) shouldBe true
  }

  property("Evaluate array ops") {
    testEval("""{
              |  val arr = Array(1, 2) ++ Array(3, 4)
              |  arr.size == 4
              |}""".stripMargin)
    testEval("""{
              |  val arr = Array(1, 2, 3)
              |  arr.slice(1, 3) == Array(2, 3)
              |}""".stripMargin)
    testEval("""{
              |  val arr = bytes1 ++ bytes2
              |  arr.size == 6
              |}""".stripMargin)
    testEval("""{
              |  val arr = bytes1 ++ Array[Byte]()
              |  arr.size == 3
              |}""".stripMargin)
    testEval("""{
              |  val arr = Array[Byte]() ++ bytes1
              |  arr.size == 3
              |}""".stripMargin)
    testEval("""{
              |  val arr = box1.R4[Array[Int]].get
              |  arr.size == 3
              |}""".stripMargin)
    testEval("""{
              |  val arr = box1.R5[Array[Boolean]].get
              |  anyOf(arr)
              |}""".stripMargin)
    testEval("""{
              |  val arr = box1.R5[Array[Boolean]].get
              |  allOf(arr) == false
              |}""".stripMargin)
    testEval("""{
              |  val arr = Array(1, 2, 3)
              |  arr.map {(i: Int) => i + 1} == Array(2, 3, 4)
              |}""".stripMargin)
    testEval("""{
              |  val arr = Array(1, 2, 3)
              |  arr.where {(i: Int) => i < 3} == Array(1, 2)
              |}""".stripMargin)
  }

//  property("Evaluate sigma in lambdas") {
//    testeval("""{
//              |  val arr = Array(dk1, dk2)
//              |  allOf(arr.map(fun (d: Boolean) = d && true))
//              |}""".stripMargin)
//  }

  property("Evaluate arithmetic ops") {
    def testWithCasting(castSuffix: String): Unit = {
      testEval(s"1.$castSuffix + 2.$castSuffix == 3.$castSuffix")
      testEval(s"5.$castSuffix - 1.$castSuffix == 4.$castSuffix")
      testEval(s"5.$castSuffix * 2.$castSuffix == 10.$castSuffix")
      testEval(s"5.$castSuffix / 2.$castSuffix == 2.$castSuffix")
      testEval(s"5.$castSuffix % 2.$castSuffix == 1.$castSuffix")
      testEval(s"min(5.$castSuffix, 2.$castSuffix) == 2.$castSuffix")
      testEval(s"max(5.$castSuffix, 2.$castSuffix) == 5.$castSuffix")
    }
    testWithCasting("toByte")
    testWithCasting("toShort")
    testWithCasting("toInt")
    testWithCasting("toLong")
    testWithCasting("toBigInt")
  }

  property("numeric casts") {
    // downcast
    testEval("Array(1).size.toByte > 0")
    // upcast
    testEval("Array(1).size.toLong > 0")
  }

  property("failed numeric downcast (overflow)") {
    an[ArithmeticException] should be thrownBy testEval("Array(999)(0).toByte > 0")
    an[ArithmeticException] should be thrownBy testEval("Array(999)(0).toShort.toByte > 0")
    an[ArithmeticException] should be thrownBy testEval(s"Array(${Int.MaxValue})(0).toShort > 0")
    an[ArithmeticException] should be thrownBy testEval(s"Array(${Long.MaxValue}L)(0).toInt > 0")
  }

  property("string concat") {
    testEval(""" "a" + "b" == "ab" """)
    testEval(""" "a" + "b" != "cb" """)
  }

  property("fromBaseX") {
    testEval(""" fromBase58("r") == Array[Byte](49.toByte) """)
    testEval(""" fromBase64("MQ") == Array[Byte](49.toByte) """)
    testEval(""" fromBase64("M" + "Q") == Array[Byte](49.toByte) """)
  }

  property("failed fromBaseX (invalid input)") {
    an[AssertionError] should be thrownBy testEval(""" fromBase58("^%$#@").size == 3 """)
    an[IllegalArgumentException] should be thrownBy testEval(""" fromBase64("^%$#@").size == 3 """)
  }

  property("Array indexing (out of bounds with const default value)") {
    testEval("Array(1, 2).getOrElse(3, 0) == 0")
  }

  property("Array indexing (out of bounds with evaluated default value)") {
    testEval("Array(1, 1).getOrElse(3, 1 + 1) == 2")
  }

  property("Evaluation example #1") {
    val dk1 = ProveDlog(secrets(0).publicImage.h)
    val dk2 = ProveDlog(secrets(1).publicImage.h)

    val env1 = TestingContext(99)
    val env2 = TestingContext(101)

    val prop = OR(
      AND(LE(Height, LongConstant(100)), AND(dk1, dk2)),
      AND(GT(Height, LongConstant(100)), dk1)
    )

    val challenge = Array.fill(32)(Random.nextInt(100).toByte)

    val proof1 = TestingInterpreter.prove(prop, env1, challenge).get.proof

    verify(prop, env1, proof1, challenge).map(_._1).getOrElse(false) shouldBe true

    verify(prop, env2, proof1, challenge).map(_._1).getOrElse(false) shouldBe false
  }

  property("Evaluation - no real proving - true case") {
    val prop1 = TrueLeaf

    val challenge = Array.fill(32)(Random.nextInt(100).toByte)
    val proof = NoProof
    val env = TestingContext(99)

    verify(prop1, env, proof, challenge).map(_._1).getOrElse(false) shouldBe true

    val prop2 = OR(TrueLeaf, FalseLeaf)
    verify(prop2, env, proof, challenge).map(_._1).getOrElse(false) shouldBe true

    val prop3 = AND(TrueLeaf, TrueLeaf)
    verify(prop3, env, proof, challenge).map(_._1).getOrElse(false) shouldBe true

    val prop4 = GT(Height, LongConstant(90))
    verify(prop4, env, proof, challenge).map(_._1).getOrElse(false) shouldBe true
  }

  property("Evaluation - no real proving - false case") {
    val prop1 = FalseLeaf

    val challenge = Array.fill(32)(Random.nextInt(100).toByte)
    val proof = NoProof
    val env = TestingContext(99)

    verify(prop1, env, proof, challenge).map(_._1).getOrElse(false) shouldBe false

    val prop2 = OR(FalseLeaf, FalseLeaf)
    verify(prop2, env, proof, challenge).map(_._1).getOrElse(false) shouldBe false

    val prop3 = AND(FalseLeaf, TrueLeaf)
    verify(prop3, env, proof, challenge).map(_._1).getOrElse(false) shouldBe false

    val prop4 = GT(Height, LongConstant(100))
    verify(prop4, env, proof, challenge).map(_._1).getOrElse(false) shouldBe false
  }

  property("Evaluation - hash function") {
    val bytes = "hello world".getBytes
    val hash = Blake2b256(bytes)

    val prop1 = EQ(CalcBlake2b256(ByteArrayConstant(bytes)), ByteArrayConstant(hash))

    val challenge = Array.fill(32)(Random.nextInt(100).toByte)
    val proof = NoProof
    val env = TestingContext(99)

    verify(prop1, env, proof, challenge).map(_._1).getOrElse(false) shouldBe true

    val prop2 = NEQ(CalcBlake2b256(ByteArrayConstant(bytes)), ByteArrayConstant(hash))

    verify(prop2, env, proof, challenge).map(_._1).getOrElse(false) shouldBe false

    val prop3 = EQ(CalcBlake2b256(ByteArrayConstant(bytes)), ByteArrayConstant(bytes))

    verify(prop3, env, proof, challenge).map(_._1).getOrElse(false) shouldBe false
  }

  property("passing a lambda argument") {
    // single expression
    testEval(
      """ Array[Int](1,2,3).map { (a: Int) =>
        |   a + 1
        | } == Array[Int](2,3,4) """.stripMargin)
    // block
    testEval(
      """ Array[Int](1,2,3).map { (a: Int) =>
        |   val b = a - 1
        |   b + 2
        | } == Array[Int](2,3,4) """.stripMargin)
    // block with nested lambda
    testEval(
      """ Array[Int](1,2,3).exists { (a: Int) =>
        |   Array[Int](1).exists{ (c: Int) => c == 1 }
        | } == true """.stripMargin)

    // block with nested lambda (assigned to a val)
    testEval(
      """ Array[Int](1,2,3).exists { (a: Int) =>
        |   val g = { (c: Int) => c == 1 }
        |   Array[Int](1).exists(g)
        | } == true """.stripMargin)
  }

  property("deserialize") {
    val str = Base58.encode(ValueSerializer.serialize(ByteArrayConstant(Array[Byte](2))))
    testEval(s"""deserialize[Array[Byte]]("$str").size == 1""")
    testEval(s"""deserialize[Array[Byte]]("$str")(0) == 2""")
  }
}


case class TestingContext(height: Int,
                          override val extension: ContextExtension = ContextExtension(values = Map())
                         ) extends Context[TestingContext] {
  override def withExtension(newExtension: ContextExtension): TestingContext = this.copy(extension = newExtension)

  override def toSigmaContext(IR: Evaluation): sigma.Context = ???
}

/** An interpreter for tests with 2 random secrets*/
object TestingInterpreter extends Interpreter with ProverInterpreter {
  override type CTX = TestingContext

  override val maxCost = CostTable.ScriptLimit

  override lazy val secrets: Seq[DLogProverInput] =
    Seq(DLogProverInput.random(), DLogProverInput.random())

//  override val contextExtenders: Map[Byte, CollectionConstant[SByte.type]] = Map[Byte, CollectionConstant[SByte.type]]()

  override def evaluateNode(context: TestingContext, tree: SValue): SValue = tree match {
    case Height => LongConstant(context.height)
    case _ => super.evaluateNode(context, tree)
  }
}
