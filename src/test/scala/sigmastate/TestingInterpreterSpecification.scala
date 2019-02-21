package sigmastate

import sigmastate.basics.DLogProtocol.{ProveDlog, DLogProverInput}
import scorex.crypto.hash.Blake2b256
import sigmastate.Values._
import sigmastate.interpreter._
import Interpreter._
import sigmastate.utxo.ErgoLikeTestInterpreter
import sigmastate.lang.Terms._
import org.ergoplatform._
import scorex.util.encode.Base58
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.serialization.ValueSerializer
import TrivialProp._

import scala.util.Random

class TestingInterpreterSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext
  lazy val prover = new ErgoLikeTestProvingInterpreter()
  lazy val verifier = new ErgoLikeTestInterpreter

  implicit val soundness = CryptoConstants.soundnessBits
  
  def testingContext(h: Int) =
    ErgoLikeContext(h,
      AvlTreeData.dummy, ErgoLikeContext.dummyPubkey, IndexedSeq(fakeSelf),
      ErgoLikeTransaction(IndexedSeq.empty, IndexedSeq.empty),
      fakeSelf)

  property("Reduction to crypto #1") {
    forAll() { (h: Int) =>
      whenever(h > 0 && h < Int.MaxValue - 1) {
        val dk1 = SigmaPropConstant(DLogProverInput.random().publicImage).isProven

        val ctx = testingContext(h)
        prover.reduceToCrypto(ctx, AND(GE(Height, IntConstant(h - 1)), dk1)).get._1 should(
          matchPattern { case sb: SigmaBoolean => })
        prover.reduceToCrypto(ctx, AND(GE(Height, IntConstant(h)), dk1)).get._1 should (
          matchPattern { case sb: SigmaBoolean => })

        {
          val res = prover.reduceToCrypto(ctx, AND(GE(Height, IntConstant(h + 1)), dk1)).get._1
          res should matchPattern { case FalseProp => }
        }

        {
          val res = prover.reduceToCrypto(ctx, OR(GE(Height, IntConstant(h - 1)), dk1)).get._1
          res should matchPattern { case TrueProp => }
        }

        {
          val res = prover.reduceToCrypto(ctx, OR(GE(Height, IntConstant(h)), dk1)).get._1
          res should matchPattern { case TrueProp => }
        }
        {
          val res = prover.reduceToCrypto(ctx, OR(GE(Height, IntConstant(h + 1)), dk1)).get._1
          res should matchPattern { case sb: SigmaBoolean => }
        }
      }
    }
  }

  property("Reduction to crypto #2") {
    forAll() { (h: Int) =>

      whenever(h > 0 && h < Int.MaxValue - 1) {

        val dk1 = DLogProverInput.random().publicImage.isProven
        val dk2 = DLogProverInput.random().publicImage.isProven

        val ctx = testingContext(h)

        assert(prover.reduceToCrypto(ctx, OR(
                  AND(LE(Height, IntConstant(h + 1)), AND(dk1, dk2)),
                  AND(GT(Height, IntConstant(h + 1)), dk1)
                )).get._1.isInstanceOf[CAND])


        assert(prover.reduceToCrypto(ctx, OR(
                  AND(LE(Height, IntConstant(h - 1)), AND(dk1, dk2)),
                  AND(GT(Height, IntConstant(h - 1)), dk1)
                )).get._1.isInstanceOf[ProveDlog])

        prover.reduceToCrypto(ctx, OR(
          AND(LE(Height, IntConstant(h - 1)), AND(dk1, dk2)),
          AND(GT(Height, IntConstant(h + 1)), dk1)
        )).get._1 shouldBe FalseProp

        prover.reduceToCrypto(ctx,
          OR(
            OR(
              AND(LE(Height, IntConstant(h - 1)), AND(dk1, dk2)),
              AND(GT(Height, IntConstant(h + 1)), dk1)
            ),
            AND(GT(Height, IntConstant(h - 1)), LE(Height, IntConstant(h + 1)))
          )
        ).get._1 shouldBe TrueProp

      }
    }
  }

  def testEval(code: String) = {
    val reg1 = ErgoBox.nonMandatoryRegisters.head
    val reg2 = ErgoBox.nonMandatoryRegisters(1)

    val dk1 = prover.dlogSecrets(0).publicImage
    val dk2 = prover.dlogSecrets(1).publicImage
    val ctx = testingContext(99)
    val env = Map(
      "dk1" -> dk1,
      "dk2" -> dk2,
      "bytes1" -> Array[Byte](1, 2, 3),
      "bytes2" -> Array[Byte](4, 5, 6),
      "box1" -> ErgoBox(10, ErgoScriptPredef.TrueProp, 0, Seq(), Map(
          reg1 -> IntArrayConstant(Array[Int](1, 2, 3)),
          reg2 -> BoolArrayConstant(Array[Boolean](true, false, true)))))
    val prop = compileWithCosting(env, code).asBoolValue.toSigmaProp
    println(code)
    println(prop)
    val challenge = Array.fill(32)(Random.nextInt(100).toByte)
    val proof1 = prover.prove(prop, ctx, challenge).get.proof
    verifier.verify(Interpreter.emptyEnv, prop, ctx, proof1, challenge).map(_._1).getOrElse(false) shouldBe true
  }

  property("Evaluate array ops") {
    testEval(
      """{
        |  val arr = Coll(1, 2) ++ Coll(3, 4)
        |  arr.size == 4
        |}""".stripMargin)
    testEval(
      """{
        |  val arr = Coll(1, 2, 3)
        |  arr.slice(1, 3) == Coll(2, 3)
        |}""".stripMargin)
    testEval(
      """{
        |  val arr = bytes1 ++ bytes2
        |  arr.size == 6
        |}""".stripMargin)
    testEval(
      """{
        |  val arr = bytes1 ++ Coll[Byte]()
        |  arr.size == 3
        |}""".stripMargin)
    testEval(
      """{
        |  val arr = Coll[Byte]() ++ bytes1
        |  arr.size == 3
        |}""".stripMargin)
    testEval(
      """{
        |  val arr = box1.R4[Coll[Int]].get
        |  arr.size == 3
        |}""".stripMargin)
    testEval(
      """{
        |  val arr = box1.R5[Coll[Boolean]].get
        |  anyOf(arr)
        |}""".stripMargin)
    testEval(
      """{
        |  val arr = box1.R5[Coll[Boolean]].get
        |  allOf(arr) == false
        |}""".stripMargin)

    testEval(
      """{
        |  val arr = Coll(1, 2, 3)
        |  arr.size == 3
        |}""".stripMargin)
    testEval(
      """{
        |  val arr = Coll(true, false)
        |  anyOf(arr)
        |}""".stripMargin)
    testEval(
      """{
        |  val arr = Coll(true, false)
        |  allOf(arr) == false
        |}""".stripMargin)
    testEval(
      """{
        |  val arr = Coll(1, 2, 3)
        |  arr.map {(i: Int) => i + 1} == Coll(2, 3, 4)
        |}""".stripMargin)
    //    // TODO uncomment when Costing for where is implemented
    //    testEval("""{
    //              |  val arr = Array(1, 2, 3)
    //              |  arr.filter {(i: Int) => i < 3} == Array(1, 2)
    //              |}""".stripMargin)
  }

//  property("Evaluate sigma in lambdas") {
//    testeval("""{
//              |  val arr = Array(dk1, dk2)
//              |  allOf(arr.map(fun (d: Boolean) = d && true))
//              |}""".stripMargin)
//  }

  property("Evaluate numeric casting ops") {
    def testWithCasting(castSuffix: String): Unit = {
      testEval(s"Coll(1).size.toByte.$castSuffix == 1.$castSuffix")
      testEval(s"Coll(1).size.toShort.$castSuffix == 1.$castSuffix")
      testEval(s"Coll(1).size.toInt.$castSuffix == 1.$castSuffix")
      testEval(s"Coll(1).size.toLong.$castSuffix == 1.$castSuffix")
    }
    testWithCasting("toByte")
    testWithCasting("toShort")
    testWithCasting("toInt")
    testWithCasting("toLong")
    testWithCasting("toBigInt")
  }

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

  property("failed numeric downcast (overflow)") {
    assertExceptionThrown(testEval("Coll(999)(0).toByte > 0"),
      _.getCause.isInstanceOf[ArithmeticException])
    assertExceptionThrown(testEval("Coll(999)(0).toShort.toByte > 0"),
      _.getCause.isInstanceOf[ArithmeticException])
    assertExceptionThrown(testEval(s"Coll(${Int.MaxValue})(0).toShort > 0"),
      _.getCause.isInstanceOf[ArithmeticException])
    assertExceptionThrown(testEval(s"Coll(${Long.MaxValue}L)(0).toInt > 0"),
      _.getCause.isInstanceOf[ArithmeticException])
  }

  property("Coll indexing (out of bounds with const default value)") {
    testEval("Coll(1, 2).getOrElse(3, 0) == 0")
  }

  property("Coll indexing (out of bounds with evaluated default value)") {
    testEval("Coll(1, 1).getOrElse(3, 1 + 1) == 2")
  }

  property("Evaluation example #1") {
    val dk1 = prover.dlogSecrets(0).publicImage.isProven
    val dk2 = prover.dlogSecrets(1).publicImage.isProven

    val env1 = testingContext(99)
    val env2 = testingContext(101)

    val prop = OR(
      AND(LE(Height, IntConstant(100)), AND(dk1, dk2)),
      AND(GT(Height, IntConstant(100)), dk1)
    ).toSigmaProp

    val challenge = Array.fill(32)(Random.nextInt(100).toByte)

    val proof1 = prover.prove(prop, env1, challenge).get.proof

    verifier.verify(emptyEnv, prop, env1, proof1, challenge).map(_._1).getOrElse(false) shouldBe true

    verifier.verify(emptyEnv, prop, env2, proof1, challenge).map(_._1).getOrElse(false) shouldBe false
  }

  property("Evaluation - no real proving - true case") {
    val prop1 = ErgoScriptPredef.TrueProp

    val challenge = Array.fill(32)(Random.nextInt(100).toByte)
    val proof = NoProof
    val env = testingContext(99)

    verifier.verify(prop1, env, proof, challenge).map(_._1).fold(t => throw t, identity) shouldBe true

    val prop2 = OR(TrueLeaf, FalseLeaf).toSigmaProp
    verifier.verify(prop2, env, proof, challenge).map(_._1).fold(t => throw t, identity) shouldBe true

    val prop3 = AND(TrueLeaf, TrueLeaf).toSigmaProp
    verifier.verify(prop3, env, proof, challenge).map(_._1).fold(t => throw t, identity) shouldBe true

    val prop4 = GT(Height, IntConstant(90)).toSigmaProp
    verifier.verify(prop4, env, proof, challenge).map(_._1).fold(t => throw t, identity) shouldBe true
  }

  property("Evaluation - no real proving - false case") {
    val prop1 = ErgoScriptPredef.FalseProp

    val challenge = Array.fill(32)(Random.nextInt(100).toByte)
    val proof = NoProof
    val env = testingContext(99)

    verifier.verify(prop1, env, proof, challenge).map(_._1).fold(t => throw t, identity) shouldBe false

    val prop2 = OR(FalseLeaf, FalseLeaf).toSigmaProp
    verifier.verify(prop2, env, proof, challenge).map(_._1).fold(t => throw t, identity) shouldBe false

    val prop3 = AND(FalseLeaf, TrueLeaf).toSigmaProp
    verifier.verify(prop3, env, proof, challenge).map(_._1).fold(t => throw t, identity) shouldBe false

    val prop4 = GT(Height, LongConstant(100)).toSigmaProp
    verifier.verify(prop4, env, proof, challenge).map(_._1).fold(t => throw t, identity) shouldBe false
  }

  property("Evaluation - hash function") {
    val bytes = "hello world".getBytes
    val hash = Blake2b256(bytes)

    val prop1 = EQ(CalcBlake2b256(ByteArrayConstant(bytes)), ByteArrayConstant(hash)).toSigmaProp

    val challenge = Array.fill(32)(Random.nextInt(100).toByte)
    val proof = NoProof
    val env = testingContext(99)

    verifier.verify(prop1, env, proof, challenge).map(_._1).getOrElse(false) shouldBe true

    val prop2 = NEQ(CalcBlake2b256(ByteArrayConstant(bytes)), ByteArrayConstant(hash)).toSigmaProp

    verifier.verify(prop2, env, proof, challenge).map(_._1).getOrElse(false) shouldBe false

    val prop3 = EQ(CalcBlake2b256(ByteArrayConstant(bytes)), ByteArrayConstant(bytes)).toSigmaProp

    verifier.verify(prop3, env, proof, challenge).map(_._1).getOrElse(false) shouldBe false
  }

  property("passing a lambda argument") {
    // single expression
    testEval(
      """ Coll[Int](1,2,3).map { (a: Int) =>
        |   a + 1
        | } == Coll[Int](2,3,4) """.stripMargin)
    // block
    testEval(
      """ Coll[Int](1,2,3).map { (a: Int) =>
        |   val b = a - 1
        |   b + 2
        | } == Coll[Int](2,3,4) """.stripMargin)
  }

  property("nested lambdas argument") {
    // block with nested lambda
    testEval(
      """ Coll[Int](1,2,3).exists { (a: Int) =>
        |   Coll[Int](1).exists{ (c: Int) => c == 1 }
        | } == true """.stripMargin)

    // block with nested lambda (assigned to a val)
    testEval(
      """ Coll[Int](1,2,3).exists { (a: Int) =>
        |   def g(c: Int) = c == 1
        |   Coll[Int](1).exists(g)
        | } == true """.stripMargin)
  }

  property("deserialize") {
    val str = Base58.encode(ValueSerializer.serialize(ByteArrayConstant(Array[Byte](2))))
    testEval(s"""deserialize[Coll[Byte]]("$str")(0) == 2""")
  }
}

