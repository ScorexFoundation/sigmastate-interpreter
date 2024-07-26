package sigmastate

import sigmastate.crypto.DLogProtocol.DLogProverInput
import scorex.crypto.hash.Blake2b256
import sigma.ast._
import sigma.ast.syntax._
import sigmastate.interpreter._
import Interpreter._
import io.circe.parser.parse
import org.ergoplatform._
import org.ergoplatform.sdk.JsonCodecs
import org.scalatest.BeforeAndAfterAll
import scorex.util.encode.Base58
import sigma.Colls
import sigma.VersionContext.V6SoftForkVersion
import sigma.VersionContext
import sigma.crypto.CryptoConstants
import sigma.data.{CAND, CAvlTree, ProveDlog, SigmaBoolean, TrivialProp}
import sigma.interpreter.ContextExtension
import sigma.util.Extensions.IntOps
import sigmastate.helpers.{CompilerTestingCommons, ErgoLikeContextTesting, ErgoLikeTestInterpreter, ErgoLikeTestProvingInterpreter}
import sigmastate.helpers.TestingHelpers._
import sigma.serialization.{GroupElementSerializer, SigmaSerializer, ValueSerializer}
import sigmastate.eval.CPreHeader
import sigmastate.helpers.ErgoLikeContextTesting.noBoxes
import sigmastate.interpreter.CErgoTreeEvaluator.DefaultEvalSettings
import sigmastate.utils.Helpers._
import sigma.util.Extensions._

import scala.util.Random

class TestingInterpreterSpecification extends CompilerTestingCommons
  with CompilerCrossVersionProps with BeforeAndAfterAll {

  val IR: TestingIRContext = new TestingIRContext

  lazy val prover = new ErgoLikeTestProvingInterpreter()

  lazy val verifier = new ErgoLikeTestInterpreter
  
  def testingContext(h: Int = 614401) = {

    // valid header from Ergo blockchain
    val headerJson =
      """
        |{
        |  "extensionId" : "00cce45975d87414e8bdd8146bc88815be59cd9fe37a125b5021101e05675a18",
        |  "votes" : "000000",
        |  "timestamp" : 4928911477310178288,
        |  "size" : 223,
        |  "unparsedBytes" : "",
        |  "stateRoot" : {
        |      "digest" : "5c8c00b8403d3701557181c8df800001b6d5009e2201c6ff807d71808c00019780",
        |      "treeFlags" : "0",
        |      "keyLength" : "32"
        |  },
        |  "height" : 614400,
        |  "nBits" : 37748736,
        |  "version" : 2,
        |  "id" : "5603a937ec1988220fc44fb5022fb82d5565b961f005ebb55d85bd5a9e6f801f",
        |  "adProofsRoot" : "5d3f80dcff7f5e7f59007294c180808d0158d1ff6ba10000f901c7f0ef87dcff",
        |  "transactionsRoot" : "f17fffacb6ff7f7f1180d2ff7f1e24ffffe1ff937f807f0797b9ff6ebdae007e",
        |  "extensionRoot" : "1480887f80007f4b01cf7f013ff1ffff564a0000b9a54f00770e807f41ff88c0",
        |  "minerPk" : "03bedaee069ff4829500b3c07c4d5fe6b3ea3d3bf76c5c28c1d4dcdb1bed0ade0c",
        |  "powOnetimePk" : "0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798",
        |  "powNonce" : "0000000000003105",
        |  "powDistance" : 0,
        |  "adProofsId" : "dec129290a763f4de41f04e87e2b661dd59758af6bdd00dd51f5d97c3a8cb9b5",
        |  "transactionsId" : "eba1dd82cf51147232e09c1f72b37c554c30f63274d5093bff36849a83472a42",
        |  "parentId" : "ac2101807f0000ca01ff0119db227f202201007f62000177a080005d440896d0"
        |}
        |""".stripMargin

    object JsonCodecs extends JsonCodecs
    val header1 = JsonCodecs.headerDecoder.decodeJson(parse(headerJson).toOption.get).toOption.get

    val boxesToSpend = IndexedSeq(fakeSelf)

    val preHeader = CPreHeader(activatedVersionInTests,
      parentId = header1.id,
      timestamp = 3,
      nBits = 0,
      height = h,
      minerPk = GroupElementSerializer.parse(SigmaSerializer.startReader(ErgoLikeContextTesting.dummyPubkey)).toGroupElement,
      votes = Colls.emptyColl[Byte]
    )

    new ErgoLikeContext(
      header1.stateRoot.asInstanceOf[CAvlTree].treeData, Colls.fromArray(Array(header1)),
      preHeader, noBoxes,
      boxesToSpend, ErgoLikeTransaction(IndexedSeq.empty, IndexedSeq.empty),
      boxesToSpend.indexOf(fakeSelf), ContextExtension.empty, vs, DefaultEvalSettings.scriptCostLimitInEvaluator,
      initCost = 0L, activatedVersionInTests).withErgoTreeVersion(ergoTreeVersionInTests)
  }

  property("Reduction to crypto #1") {
    forAll() { i: Int =>
      val h = i.toAbs
      whenever(h > 0 && h < Int.MaxValue - 1) {
        val dk1 = SigmaPropConstant(DLogProverInput.random().publicImage)

        val ctx = testingContext(h)
        testReduce(prover)(ctx, SigmaAnd(GE(Height, IntConstant(h - 1)), dk1)) should(
          matchPattern { case _: SigmaBoolean => })
        testReduce(prover)(ctx, SigmaAnd(GE(Height, IntConstant(h)), dk1)) should (
          matchPattern { case _: SigmaBoolean => })

        {
          val res = testReduce(prover)(ctx, SigmaAnd(GE(Height, IntConstant(h + 1)), dk1))
          res should matchPattern { case TrivialProp.FalseProp => }
        }

        {
          val res = testReduce(prover)(ctx, SigmaOr(GE(Height, IntConstant(h - 1)), dk1))
          res should matchPattern { case TrivialProp.TrueProp => }
        }

        {
          val res = testReduce(prover)(ctx, SigmaOr(GE(Height, IntConstant(h)), dk1))
          res should matchPattern { case TrivialProp.TrueProp => }
        }
        {
          val res = testReduce(prover)(ctx, SigmaOr(GE(Height, IntConstant(h + 1)), dk1))
          res should matchPattern { case _: SigmaBoolean => }
        }
      }
    }
  }

  property("Reduction to crypto #2") {
    forAll() { i: Int =>
      val h = i.toAbs
      whenever(h > 0 && h < Int.MaxValue - 1) {

        val dk1 = DLogProverInput.random().publicImage
        val dk2 = DLogProverInput.random().publicImage

        val ctx = testingContext(h)

        assert(testReduce(prover)(ctx, SigmaOr(
                  SigmaAnd(LE(Height, IntConstant(h + 1)), SigmaAnd(dk1, dk2)),
                  SigmaAnd(GT(Height, IntConstant(h + 1)), dk1)
                )).isInstanceOf[CAND])


        assert(testReduce(prover)(ctx, SigmaOr(
                  SigmaAnd(LE(Height, IntConstant(h - 1)), SigmaAnd(dk1, dk2)),
                  SigmaAnd(GT(Height, IntConstant(h - 1)), dk1)
                )).isInstanceOf[ProveDlog])

        testReduce(prover)(ctx, SigmaOr(
          SigmaAnd(LE(Height, IntConstant(h - 1)), SigmaAnd(dk1, dk2)),
          SigmaAnd(GT(Height, IntConstant(h + 1)), dk1)
        )) shouldBe TrivialProp.FalseProp

        testReduce(prover)(ctx,
          SigmaOr(
            SigmaOr(
              SigmaAnd(LE(Height, IntConstant(h - 1)), SigmaAnd(dk1, dk2)),
              SigmaAnd(GT(Height, IntConstant(h + 1)), dk1)
            ),
            AND(GT(Height, IntConstant(h - 1)), LE(Height, IntConstant(h + 1)))
          )
        ) shouldBe TrivialProp.TrueProp

      }
    }
  }

  def testEval(code: String) = {
    val reg1 = ErgoBox.nonMandatoryRegisters.head
    val reg2 = ErgoBox.nonMandatoryRegisters(1)

    val dk1 = prover.dlogSecrets(0).publicImage
    val dk2 = prover.dlogSecrets(1).publicImage
    val ctx = testingContext()
    val env = Map(
      "dk1" -> dk1,
      "dk2" -> dk2,
      "bytes1" -> Array[Byte](1, 2, 3),
      "bytes2" -> Array[Byte](4, 5, 6),
      "box1" -> testBox(10, TrueTree, 0, Seq(), Map(
          reg1 -> IntArrayConstant(Array[Int](1, 2, 3)),
          reg2 -> BoolArrayConstant(Array[Boolean](true, false, true))
      ))
    )
    val prop = mkTestErgoTree(compile(env, code)(IR).asBoolValue.toSigmaProp)
    val challenge = Array.fill(32)(Random.nextInt(100).toByte)
    val proof1 = prover.prove(prop, ctx, challenge).get.proof
    verifier.verify(Interpreter.emptyEnv, prop, ctx, proof1, challenge)
      .map(_._1)
      .getOrElse(false) shouldBe true
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
    testEval(
      """{
        |  val arr = Coll(1, 2, 3)
        |  arr.filter {(i: Int) => i < 3} == Coll(1, 2)
        |}""".stripMargin)
  }

  property("Evaluate numeric casting ops") {
    def testWithCasting(castSuffix: String): Unit = {
      testEval(s"OUTPUTS.size.toByte.$castSuffix == 0.$castSuffix")
      testEval(s"OUTPUTS.size.toShort.$castSuffix == 0.$castSuffix")
      testEval(s"OUTPUTS.size.toInt.$castSuffix == 0.$castSuffix")
      testEval(s"OUTPUTS.size.toLong.$castSuffix == 0.$castSuffix")
    }
    testWithCasting("toByte")
    testWithCasting("toShort")
    testWithCasting("toInt")
    testWithCasting("toLong")
    testWithCasting("toBigInt")
  }

  property("BigInt downcasting to byte") {
    def test() = testEval("{ sigmaProp(0L.toBigInt.toByte <= CONTEXT.preHeader.version) }")
    if(VersionContext.current.isV6SoftForkActivated) {
      test()
    } else {
      an[Exception] shouldBe thrownBy(test())
    }
  }

  property("BigInt downcasting to short") {
    def test() = testEval("{ sigmaProp(0L.toBigInt.toShort <= CONTEXT.preHeader.version.toShort) }")
    if(VersionContext.current.isV6SoftForkActivated) {
      test()
    } else {
      an[Exception] shouldBe thrownBy(test())
    }
  }

  property("BigInt downcasting to int") {
    def test() = testEval("{ sigmaProp(1L.toBigInt.toInt < CONTEXT.preHeader.timestamp.toInt) }")
    if(VersionContext.current.isV6SoftForkActivated) {
      test()
    } else {
      an[Exception] shouldBe thrownBy(test())
    }
  }

  property("BigInt downcasting to long") {
    def test() = testEval("{ sigmaProp(1L.toBigInt.toLong < CONTEXT.preHeader.timestamp) }")
    if(VersionContext.current.isV6SoftForkActivated) {
      test()
    } else {
      an[Exception] shouldBe thrownBy(test())
    }
  }

  property("upcasting to bigint") {
    testEval("{ sigmaProp(1L.toBigInt < bigInt(\"2\")) }")
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
      rootCause(_).isInstanceOf[ArithmeticException])
    assertExceptionThrown(testEval("Coll(999)(0).toShort.toByte > 0"),
      rootCause(_).isInstanceOf[ArithmeticException])
    assertExceptionThrown(testEval(s"Coll(${Int.MaxValue})(0).toShort > 0"),
      rootCause(_).isInstanceOf[ArithmeticException])
    assertExceptionThrown(testEval(s"Coll(${Long.MaxValue}L)(0).toInt > 0"),
      rootCause(_).isInstanceOf[ArithmeticException])
  }

  property("Coll indexing (out of bounds with const default value)") {
    testEval("Coll(1, 2).getOrElse(3, 0) == 0")
  }

  property("Coll indexing (out of bounds with evaluated default value)") {
    testEval("Coll(1, 1).getOrElse(3, 1 + 1) == 2")
  }

  property("Evaluation example #1") {
    val dk1 = prover.dlogSecrets(0).publicImage
    val dk2 = prover.dlogSecrets(1).publicImage

    val env1 = testingContext(99)
    val env2 = testingContext(101)

    val prop = mkTestErgoTree(SigmaOr(
      SigmaAnd(LE(Height, IntConstant(100)), SigmaAnd(dk1, dk2)),
      SigmaAnd(GT(Height, IntConstant(100)), dk1)
    ))

    val challenge = Array.fill(32)(Random.nextInt(100).toByte)

    val proof1 = prover.prove(prop, env1, challenge).get.proof

    verifier.verify(emptyEnv, prop, env1, proof1, challenge).map(_._1).getOrElse(false) shouldBe true

    verifier.verify(emptyEnv, prop, env2, proof1, challenge).map(_._1).getOrElse(false) shouldBe false
  }

  property("Evaluation - no real proving - true case") {
    val prop1 = TrueTree

    val challenge = Array.fill(32)(Random.nextInt(100).toByte)
    val proof = NoProof
    val env = testingContext(99)

    verifier.verify(prop1, env, proof, challenge).map(_._1).getOrThrow shouldBe true

    val prop2 = mkTestErgoTree(OR(TrueLeaf, FalseLeaf).toSigmaProp)
    verifier.verify(prop2, env, proof, challenge).map(_._1).getOrThrow shouldBe true

    val prop3 = mkTestErgoTree(AND(TrueLeaf, TrueLeaf).toSigmaProp)
    verifier.verify(prop3, env, proof, challenge).map(_._1).getOrThrow shouldBe true

    val prop4 = mkTestErgoTree(GT(Height, IntConstant(90)).toSigmaProp)
    verifier.verify(prop4, env, proof, challenge).map(_._1).getOrThrow shouldBe true
  }

  property("Evaluation - no real proving - false case") {
    val prop1 = FalseTree

    val challenge = Array.fill(32)(Random.nextInt(100).toByte)
    val proof = NoProof
    val env = testingContext(99)

    verifier.verify(prop1, env, proof, challenge).map(_._1).getOrThrow shouldBe false

    val prop2 = mkTestErgoTree(OR(FalseLeaf, FalseLeaf).toSigmaProp)
    verifier.verify(prop2, env, proof, challenge).map(_._1).getOrThrow shouldBe false

    val prop3 = mkTestErgoTree(AND(FalseLeaf, TrueLeaf).toSigmaProp)
    verifier.verify(prop3, env, proof, challenge).map(_._1).getOrThrow shouldBe false

    val prop4 = mkTestErgoTree(GT(Height, IntConstant(100)).toSigmaProp)
    verifier.verify(prop4, env, proof, challenge).map(_._1).getOrThrow shouldBe false
  }

  property("Evaluation - hash function") {
    val bytes = "hello world".getBytes
    val hash = Blake2b256(bytes)

    val prop1 = mkTestErgoTree(EQ(
      CalcBlake2b256(ByteArrayConstant(bytes)),
      ByteArrayConstant(hash)).toSigmaProp)

    val challenge = Array.fill(32)(Random.nextInt(100).toByte)
    val proof = NoProof
    val env = testingContext(99)

    verifier.verify(prop1, env, proof, challenge).map(_._1).getOrElse(false) shouldBe true

    val prop2 = mkTestErgoTree(NEQ(
      CalcBlake2b256(ByteArrayConstant(bytes)),
      ByteArrayConstant(hash)).toSigmaProp)

    verifier.verify(prop2, env, proof, challenge).map(_._1).getOrElse(false) shouldBe false

    val prop3 = mkTestErgoTree(EQ(
      CalcBlake2b256(ByteArrayConstant(bytes)),
      ByteArrayConstant(bytes)).toSigmaProp)

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

  override protected def afterAll(): Unit = {
  }

}

