package sigmastate.utxo

import org.ergoplatform.ErgoBox._
import org.ergoplatform.ErgoConstants.ScriptCostLimit
import org.ergoplatform._
import scalan.util.BenchmarkUtil
import scalan.util.BenchmarkUtil.measureTime
import scorex.crypto.hash.Blake2b256
import sigmastate.Values._
import sigmastate._
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval._
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeTestInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.CryptoConstants.dlogGroup
import sigmastate.interpreter.Interpreter._
import sigmastate.interpreter.{ContextExtension, CostedProverResult}
import sigmastate.lang.Terms._
import sigmastate.serialization.ErgoTreeSerializer
import sigmastate.serialization.generators.ObjectGenerators

import scala.util.Try

/**
  * Suite of tests where a malicious prover tries to feed a verifier with a script which is costly to verify
  */
class MiscErrors extends SigmaTestingCommons with ObjectGenerators {
  implicit lazy val IR: TestingIRContext = new TestingIRContext {
    saveGraphsInFile = false
  }

  //we assume that verifier must finish verification of any script in less time than 1M hash calculations
  // (for the Blake2b256 hash function over a single block input)
  val Timeout: Long = {
    val block = Array.fill(16)(0: Byte)
    val hf = Blake2b256

    //just in case to heat up JVM
    (1 to 1000000).foreach(_ => hf(block))

    val t0 = System.currentTimeMillis()
    (1 to 1000000).foreach(_ => hf(block))
    val t = System.currentTimeMillis()
    t - t0
  }
  val NumInputs: Int = 10
  val NumOutputs: Int = 10
  val InputCostDefault: Int = 2000
  val CostLimit: Long = ScriptCostLimit.value
  val Longs: Array[Long] = Array[Long](1, 2, 3, Long.MaxValue, Long.MinValue)
  lazy val alice = new ContextEnrichingTestProvingInterpreter
  lazy val alicePubKey: ProveDlog = alice.dlogSecrets.head.publicImage
  // script of maximum size
  val maxSizeColl: Array[Byte] = Array.fill(MaxBoxSize)(2.toByte)
  val coll10: Array[Byte] = Array.fill(10)(10.toByte)
  lazy val maxSizeCollEnv: ScriptEnv = Map(
    "alice" -> alice.dlogSecrets.head.publicImage,
    "alice2" -> alice.dlogSecrets(1).publicImage,
    "alice3" -> alice.dlogSecrets(2).publicImage,
    "alice4" -> alice.dlogSecrets(3).publicImage,
    "coll5" -> Colls.fromArray(Array.fill(5)(5.toByte)),
    "coll10" -> Colls.fromArray(coll10),
    "coll100" -> Colls.fromArray(Array.fill(100)(100.toByte)),
    "maxSizeColl" -> Colls.fromArray(maxSizeColl)
  )

  def termination[T](fn: () => T): (T, Boolean) = {
    val t0 = System.currentTimeMillis()
    val res = fn()
    val t = System.currentTimeMillis()
    (res, (t - t0) < Timeout)
  }

  def serializedScriptSize(spamScript: SigmaPropValue): Int = {
    ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(spamScript).size
  }

  def initializationCost(scriptSize: Int): Long = {
    val cost = scriptSize * CostTable.perGraphNodeCost + CostTable.interpreterInitCost
    cost
  }

  /**
    * Checks that regardless of the script structure, it's verification always consumes at most `Timeout` ms
    */
  private def checkScript(spamScript: SigmaPropValue, emptyProofs: Boolean = true): Unit = {
    val scriptSize = serializedScriptSize(spamScript)
    // TODO use MaxPropositionBytes constant here once its value can be decreased without failing tests
    assert(scriptSize <= 1500, s"Script size $scriptSize is too big, fix the test")

    val ctx = {

      val output = ErgoBox(1, alicePubKey, 10, Nil,
        Map(
          R4 -> ByteConstant(1),
          R5 -> SigmaPropConstant(alicePubKey),
          R6 -> IntConstant(10),
          R7 -> ByteArrayConstant(coll10),
          R8 -> ByteArrayConstant(maxSizeColl)
        )
      )

      val input = ErgoBox(1, spamScript, 10, Nil,
        Map(
          R4 -> ByteConstant(1),
          R5 -> SigmaPropConstant(alicePubKey),
          R6 -> IntConstant(10),
          R7 -> ByteArrayConstant(coll10),
          R8 -> ByteArrayConstant(maxSizeColl)
        )
      )
      val outBoxes: IndexedSeq[ErgoBoxCandidate] = IndexedSeq.fill(NumOutputs)(output)
      val inBoxes: IndexedSeq[ErgoBox] = IndexedSeq.fill(NumOutputs)(input)
      //normally this transaction would invalid (why?), but we're not checking it in this test
      val tx = createTransaction(outBoxes)

      ErgoLikeContext(
        currentHeight = 10,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContext.dummyPubkey,
        boxesToSpend = inBoxes,
        spendingTransaction = tx,
        self = inBoxes(0) // what is the use of self?
      )
    }

    val pr = if (emptyProofs) {
      // do not spend time to create a proof
      CostedProverResult(Array[Byte](), ContextExtension.empty, 0L)
    } else {
      // generate a correct proof using a prover without a cost limit
      val pr = new ContextEnrichingTestProvingInterpreter()
        .withSecrets(alice.dlogSecrets)
        .prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, ctx.withCostLimit(Long.MaxValue), fakeMessage).get
      println(s"Prover cost: ${pr.cost}")
      pr
    }

    val initCost = initializationCost(scriptSize)
    println(s"Initalization Cost: $initCost")
    val verifier = new ErgoLikeTestInterpreter
    val (res, calcTime) = BenchmarkUtil.measureTime {
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, ctx.withInitCost(initCost), pr, fakeMessage)
    }
    checkResult(res, calcTime, scriptSize)
  }

  def checkResult(res: Try[(Boolean, Long)], calcTime: Long, scriptSize: Int) = {
    println(s"Verify time: $calcTime millis; SerializedSize: $scriptSize")
    println(s"Timeout: $Timeout millis")
    res.fold(t => {
      val cause = rootCause(t)
      println(s"Rejection cause: $cause")
    }, r => {
      println(s"Result: $res")
    })
    calcTime should be < Timeout
    println("----------------------------")
  }

  property("not having certain types (BigInt) in env") {
    /* value y1 below is not present in environment

       Error is
         Cannot assign type for variable 'y1' because it is not found in env

       Perhaps this is expected behavior as the type of y1 is BigInt as opposed to SigmaDsl.BigInt.

     */
    checkScript(compile(
      Map(
        "x1" -> SigmaDsl.BigInt((BigInt(Blake2b256("hello"))).bigInteger),
        "y1" -> BigInt(Blake2b256("world")), // this is not present in environment
        "g1" -> dlogGroup.generator,
        "g2" -> dlogGroup.generator.add(dlogGroup.generator),
        ScriptNameProp -> "exp"
      ),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(b:Byte) =>
         |    val ex = if (b == 10) x1 else y1
         |    g1.exp(ex) != g2
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("wrong type inferred") {
    /*
    Below line val ex:BigInt = .... infers ex as Int rather than BigInt
    Need to add .toBigInt in the code for this to work.
     */
    checkScript(compile(
      Map(
        ScriptNameProp -> "exp",
        "g1" -> dlogGroup.generator,
        "g2" -> dlogGroup.generator.add(dlogGroup.generator)
      ),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(b:Byte) =>
         |    val ex:BigInt = if (b == 10) 10000 else 20000
         |    // val ex:BigInt = if (b == 10) 10000.toBigInt else 20000.toBigInt // this line works
         |    g1.exp(ex) != g2
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("large loop: exp (NoSuchMethodException)") {
    /*
      Rejection cause: java.lang.NoSuchMethodException: sigmastate.eval.CostingRules$GroupElementCoster.exp(scalan.Base$Exp)
     */
    checkScript{
      compile(
        maxSizeCollEnv ++ Map(
          ScriptNameProp -> "large loop: exp",
          "x1" -> SigmaDsl.BigInt((BigInt(Blake2b256("hello"))).bigInteger),
          "y1" -> SigmaDsl.BigInt((BigInt(Blake2b256("world"))).bigInteger),
          "g1" -> dlogGroup.generator,
          "g2" -> dlogGroup.generator.add(dlogGroup.generator)
        ),
        s"""{
           |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(b:Byte) =>
           |    val ex = if (b == 10) x1 else y1
           |    g1.exp(ex) != g2
           |  })
           |}
      """.stripMargin
      ).asBoolValue.toSigmaProp
    }
  }


}