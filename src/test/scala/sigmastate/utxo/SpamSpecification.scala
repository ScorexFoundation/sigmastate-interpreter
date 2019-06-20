package sigmastate.utxo

import org.ergoplatform.ErgoBox._
import org.ergoplatform.ErgoConstants.{MaxPropositionBytes, ScriptCostLimit}
import org.ergoplatform._
import org.ergoplatform.validation.ValidationRules
import org.scalacheck.Gen
import scalan.util.BenchmarkUtil
import scalan.util.BenchmarkUtil.measureTime
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Lookup}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.encode.Base16
import scorex.utils.Random
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.eval._
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeTestInterpreter, ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.Interpreter._
import sigmastate.interpreter.{ContextExtension, CostedProverResult}
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.CosterException
import sigmastate.serialization.ErgoTreeSerializer
import sigmastate.serialization.generators.ObjectGenerators
import special.collection.Coll

/**
  * Suite of tests where a malicious prover tries to feed a verifier with a script which is costly to verify
  */
class SpamSpecification extends SigmaTestingCommons with ObjectGenerators {
  implicit lazy val IR: TestingIRContext = new TestingIRContext

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

  /**
    * Checks that regardless of the script structure, it's verification always consumes at most `Timeout` ms
    */
  private def checkScript(spamScript: SigmaPropValue, emptyProofs: Boolean = true): Unit = {
    val size = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(spamScript).size
    assert(size <= MaxPropositionBytes.value, s"Script size $size is too big, fix the test")

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
    val verifier = new ErgoLikeTestInterpreter
    val (res, calcTime) = BenchmarkUtil.measureTime {
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, ctx, pr, fakeMessage)
    }
    println(s"Verify time: $calcTime millis")
    println(s"Timeout: $Timeout millis")
    res.fold(t => {
      val cause = rootCause(t)
      println(s"Rejection cause: $cause")
    }, r => {
      println(s"Result: $res")
    })
    val scriptCost = res.map(_._2).getOrElse(CostLimit)
    // time that will be consumed, if the whole block will be filled with such scripts
    val estimatedTime = calcTime * CostLimit / (scriptCost + InputCostDefault)
    estimatedTime should be < Timeout
  }

  def warmUpScenario() = {
    val ctx = ErgoLikeContext.dummy(fakeSelf)
    val check = "i >= 0"
    val prop = compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  maxSizeColl.forall({(i:Byte) =>
         |    $check
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp
    new ContextEnrichingTestProvingInterpreter()
      .withSecrets(alice.dlogSecrets)
      .prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx.withCostLimit(Long.MaxValue), fakeMessage).get
  }

  lazy val warmUpPrecondition = {
    val (_, t) = measureTime(warmUpScenario())
    println(s"Warmup time: $t")
    true
  }

  property("large loop: int comparison") {
    val check = "i >= 0"
    val prop = compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(i:Byte) =>
         |             $check
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp
    checkScript(prop)
  }

  property("large loop: addition") {
    val check = "i + i + i + i + i + i + i + i + i + i > i + i + i + i + i + i + i + i + i"
    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(i:Byte) =>
         |    $check
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("large loop: multiplication") {
    val check = "i * i * i * i * i * i * i * i * i * i >= i * i * i * i * i * i * i * i * i"
    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(i:Byte) =>
         |    $check
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("large loop: negotiation") {
    val check = "(! (i != 1)) || (! (i != 2)) || (! (i != 3)) || (! (i != 4))"
    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(i:Byte) =>
         |    $check
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("large loop: comparison of the same elements") {
    val check = "i == i"
    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(i:Byte) =>
         |    $check
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("large loop: blake2b256") {
    val check = "blake2b256(blake2b256(Coll(i))) != blake2b256(Coll(i))"
    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(i:Byte) =>
         |    $check
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("large loop: collection element by index") {
    val check = "OUTPUTS(0).R8[Coll[Byte]].get(i) == OUTPUTS(0).R8[Coll[Byte]].get(32700 - j)"
    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(i:Byte) =>
         |    coll5.forall({(j:Byte) =>
         |      $check
         |    })
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("large loop: collection.slice") {
    assert(warmUpPrecondition)
    val check = "maxSizeColl.slice(101, 1001) == maxSizeColl.slice(101, 1001)"
    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(i:Byte) =>
         |    coll10.forall({(j:Byte) =>
         |      $check
         |    })
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("large loop: collection.indices") {
    val check =
      s"""
         |  OUTPUTS(0).R8[Coll[Byte]].get.indices.forall({(j: Int) =>
         |   OUTPUTS(0).R8[Coll[Byte]].get.indices(j) == OUTPUTS(0).R8[Coll[Byte]].get.indices(j)
         |  })
      """
    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(i:Byte) =>
         |     $check
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("large loop: collection.filter") {
    val check = "OUTPUTS(0).R8[Coll[Byte]].get.filter({(j:Byte) => i == j}).size == 1"
    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.filter({(i:Byte) =>
         |     $check
         |  }).size == 0
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("large loop: collection allocation and comparison") {
    val check = "Coll(i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i)" +
      " != Coll(i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i)"
    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(i:Byte) =>
         |     $check
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("large loop: extract registers") {
    val check = "(SELF.R0[Long].get == SELF.R9[Long].getOrElse(SELF.R0[Long].get)) && (SELF.R4[Byte].get == (i - 1)) && INPUTS(0).R5[SigmaProp].get == OUTPUTS(0).R5[SigmaProp].get && INPUTS(0).R6[Int].get == OUTPUTS(0).R6[Int].get"
    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(i:Byte) =>
         |     $check
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("large loop: if") {
    val check =
      s"""
         |  if(i > 0) {
         |    if(i == 1) {
         |      false
         |    } else {
         |      if(i != 1) {
         |        true
         |      } else {
         |        false
         |      }
         |    }
         |  } else {
         |    false
         |  }
      """

    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(i:Byte) =>
         |     $check
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("large loop: tuple operations") {
    val check = "(i, 9223372036854775800L)._1 == (12, 2)._2 && (i, 0)._1 == (2, 0)._1 && (i, 3)._1 == (2, 0)._1"
    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(i:Byte) =>
         |     $check
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("large loop: SELF.creationInfo") {
    val check = "SELF.creationInfo._2 != Coll(i, j) && SELF.creationInfo._2 != Coll(j)"
    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(i:Byte) =>
         |    coll5.forall({(j:Byte) =>
         |     $check
         |    })
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("large loop: val allocation") {
    val check =
      s"""
         | val X = j + i
         | val Y = X + 1123 + j + j
         | val Z = Y + X + 1
         | val A = (Z * 2) + X + 1
         | val B = (A + i) % 1000
         | val C = (A + B) % 1000
         | val D = (i + C) % 1000
         | D < 1000
                    """

    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  coll1000.forall({(i:Byte) =>
         |    coll100.forall({(j:Byte) =>
         |     $check
         |    })
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("large loop: numeric casts") {
    val check = "(HEIGHT.toLong + i.toLong).toInt == (HEIGHT.toByte + i).toInt"

    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(i:Byte) =>
         |    $check
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("large loop: options") {
    val check = "getVar[Int](1).isDefined || getVar[Int](2).isDefined || SELF.R4[Int].isDefined || SELF.R0[Long].isDefined"

    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(i:Byte) =>
         |    $check
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("large loop: binary operations") {
    val check =
      s"""
         | val T1 = i == 2;
         | val T2 = (i * 3) % 5 == 1
         | val T3 = true
         | val T4 = T1 ^ false
         | val F1 = T1 ^ T2
         | val F2 = T4 && F1
         | val F3 = F1 || F2
         | val F4 = (F1 ^ F3) || (T4 ^ T3)
         | T1 && T2 && T3 && T4 && (! (F1 ^ F2)) && (! F3) && (! F4)
                    """
    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(i:Byte) =>
         |   $check
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }


  property("complex sigma proposition") {
    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> "proveDLog"),
      s"""{
         |  (alice && alice2) || (alice && alice3) || (alice && alice4) || (alice && alice3) ||
         |  allOf(Coll(alice, alice2, alice3 ,alice4)) || atLeast(2, Coll(alice, alice2, alice3 ,alice4)) ||
         |  anyOf(Coll(alice, alice2, alice3 ,alice4)) || atLeast(3, Coll(alice, alice2, alice3 ,alice4))
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("ring signature") {
    val publicImages = alice.dlogSecrets.head.publicImage +: (1 to 1000).map { _ =>
      new ContextEnrichingTestProvingInterpreter().dlogSecrets.head.publicImage
    }
    // should not consume too much time for valid number of keys in a ring
    checkScript(OR(publicImages.take(95).map(image => SigmaPropConstant(image).isProven)).toSigmaProp, emptyProofs = false)
  }


  // todo construct transaction with at least one output and check the same properties for outputs
  property("large loop: INPUTS.propositionBytes.size") {
    val check = "INPUTS.exists({(x:Box) => x.propositionBytes.size >= 0})"
    checkScript(compile(maxSizeCollEnv + (ScriptNameProp -> check),
      s"""{
         |  OUTPUTS(0).R8[Coll[Byte]].get.forall({(i:Byte) =>
         |    $check
         |  })
         |}
      """.stripMargin).asBoolValue.toSigmaProp)
  }

  property("nested loops 1") {
    val largeColl: Coll[Int] = Colls.fromArray((0 until 500).toArray)
    val env = Map(
      ScriptNameProp -> "nested loops 1",
      "largeColl" -> largeColl
    )
    val spamScript = compile(env,
      """{
        |  val valid  = largeColl.forall({(i:Int) =>
        |     largeColl.exists({(j:Int) =>
        |       i != j
        |     }
        |     ) &&
        |     largeColl.exists({(j:Int) =>
        |       largeColl.forall({(k:Int) =>
        |         k != i + j
        |       }
        |       ) &&
        |       i != j
        |     }
        |     ) &&
        |     OUTPUTS.exists({(x:Box) =>
        |       x.propositionBytes.size >= i
        |     }
        |     )
        |   }
        |  )
        |  ! valid
        |}
      """.stripMargin).asBoolValue.toSigmaProp

    checkScript(spamScript)
  }

  property("nested loops 2") {
    val largeColl = Colls.fromArray((1 to 65).toArray)
    val env = Map(
      ScriptNameProp -> "nested loops 2",
      "alice" -> alice.dlogSecrets.head.publicImage,
      "largeColl" -> largeColl
    )
    val spamScript = compile(env,
      """{
        |  val valid  = largeColl.forall({(i:Int) =>
        |     largeColl.exists({(j:Int) =>
        |       largeColl.forall({(k:Int) =>
        |         k != i + j
        |       }
        |       )
        |     }
        |     )
        |   }
        |  )
        |  alice && valid
        |}
      """.stripMargin).asBoolValue.toSigmaProp

    checkScript(spamScript)
  }

  property("large num of inputs 1") {
    // runtime is high, test failing
    /*
      TIMEOUT IS 2150
      TIME IS 598
     */
    val alice = new ContextEnrichingTestProvingInterpreter
    val alicePubKey: ProveDlog = alice.dlogSecrets.head.publicImage
    val env = Map(
      ScriptNameProp -> "Script",
      "alice" -> alicePubKey,
      "minNumInputs" -> NumInputs,
      "minNumOutputs" -> NumOutputs
    )
    val spamScript = compile(env,
      """{
        |  val valid  = INPUTS.exists({(ib:Box) =>
        |     OUTPUTS.exists({(ob:Box) =>
        |       OUTPUTS.exists({(ob2:Box) =>
        |         val ib_r4 = ib.R4[Byte].get
        |         val ib_r5 = ib.R5[SigmaProp].get
        |         val ib_r6 = ib.R6[Int].get
        |         val ib_r7 = ib.R7[Coll[Long]].get
        |         val ib_r8 = ib.R8[Coll[Byte]].get
        |         val ob_r4 = ob.R4[Byte].get
        |         val ob_r5 = ob.R5[SigmaProp].get
        |         val ob_r6 = ob.R6[Int].get
        |         val ob_r7 = ob.R7[Coll[Long]].get
        |         val ob_r8 = ob.R8[Coll[Byte]].get
        |         val ob2_r4 = ob2.R4[Byte].get
        |         val ob2_r5 = ob2.R5[SigmaProp].get
        |         val ob2_r6 = ob2.R6[Int].get
        |         val ob2_r7 = ob2.R7[Coll[Long]].get
        |         val ob2_r8 = ob2.R8[Coll[Byte]].get
        |         ib.propositionBytes == ob.propositionBytes && ob2.propositionBytes.size <= SELF.propositionBytes.size &&
        |         ib_r4 == ob_r4 && ob_r4 == ob2_r4 &&
        |         ib_r5 == ob_r5 && ob_r5 == ob2_r5 &&
        |         ib_r6 == ob_r6 && ob_r6 == ob2_r6 &&
        |         ib_r7 == ob_r7 && ob_r7 == ob2_r7 &&
        |         ib_r8 == ob_r8 && ob_r8 == ob2_r8
        |       }
        |       )
        |     }
        |     )
        |  }
        |  ) && INPUTS.size >= minNumInputs && OUTPUTS.size >= minNumOutputs
        |  alice && !valid
        |}
      """.stripMargin).asBoolValue.toSigmaProp

    val output = ErgoBox(1, alicePubKey, 10, Nil,
      Map(
        R4 -> ByteConstant(1),
        R5 -> SigmaPropConstant(alicePubKey),
        R6 -> IntConstant(10),
        R7 -> LongArrayConstant(Longs),
        R8 -> ByteArrayConstant(Base16.decode("123456123456123456123456123456123456123456123456123456123456123456").get),
      )
    )

    val input = ErgoBox(1, spamScript, 10, Nil,
      Map(
        R4 -> ByteConstant(1),
        R5 -> SigmaPropConstant(alicePubKey),
        R6 -> IntConstant(10),
        R7 -> LongArrayConstant(Longs),
        R8 -> ByteArrayConstant(Base16.decode("123456123456123456123456123456123456123456123456123456123456123456").get)
      )
    )
    val outBoxes: IndexedSeq[ErgoBoxCandidate] = IndexedSeq.fill(NumOutputs)(output)
    val inBoxes: IndexedSeq[ErgoBox] = IndexedSeq.fill(NumOutputs)(input)
    //normally this transaction would invalid (why?), but we're not checking it in this test
    val tx = createTransaction(outBoxes)

    val context = ErgoLikeContext(
      currentHeight = 10,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = inBoxes,
      spendingTransaction = tx,
      self = inBoxes(0) // what is the use of self?
    )


    val prover = new ContextEnrichingTestProvingInterpreter()

    val pr = prover.withSecrets(alice.dlogSecrets).prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, context, fakeMessage).get

    val verifier = new ErgoLikeTestInterpreter
    val (res, terminated) = termination(() =>
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, context, pr, fakeMessage)
    )
    res.isFailure shouldBe true
    terminated shouldBe true
  }

  property("large num of inputs 2") {
    /*
    Also failing, but time is high

    TIMEOUT IS 2062
    TIME IS 1476

     */
    val alice = new ContextEnrichingTestProvingInterpreter
    val alicePubKey: ProveDlog = alice.dlogSecrets.head.publicImage
    val minNumInputs = 60
    val minNumOutputs = 60
    val env = Map(
      ScriptNameProp -> "Script",
      "alice" -> alicePubKey,
      "minNumInputs" -> minNumInputs,
      "minNumOutputs" -> minNumOutputs
    )
    val spamScript = compile(env,
      """{
        |  val valid  = INPUTS.exists({(ib:Box) =>
        |     OUTPUTS.exists({(ob:Box) =>
        |         val ib_r4 = ib.R4[Byte].get
        |         val ib_r5 = ib.R5[SigmaProp].get
        |         val ib_r6 = ib.R6[Int].get
        |         val ib_r7 = ib.R7[Coll[Long]].get
        |         val ib_r8 = ib.R8[Coll[Byte]].get
        |         val ib_r9 = ib.R9[Coll[Coll[Byte]]].get
        |         val ob_r4 = ob.R4[Byte].get
        |         val ob_r5 = ob.R5[SigmaProp].get
        |         val ob_r6 = ob.R6[Int].get
        |         val ob_r7 = ob.R7[Coll[Long]].get
        |         val ob_r8 = ob.R8[Coll[Byte]].get
        |         val ob_r9 = ob.R9[Coll[Coll[Byte]]].get
        |         ib.propositionBytes == ob.propositionBytes && ob.propositionBytes.size <= SELF.propositionBytes.size &&
        |         ib_r4 == ob_r4 &&
        |         ib_r5 == ob_r5 &&
        |         ib_r6 == ob_r6 &&
        |         ib_r7 == ob_r7 &&
        |         ib_r8 == ob_r8 &&
        |         ib_r9 != ob_r9
        |     }
        |     )
        |  }
        |  ) && INPUTS.size >= minNumInputs && OUTPUTS.size >= minNumOutputs
        |  alice && !valid
        |}
      """.stripMargin).asBoolValue.toSigmaProp

    val collCollByte = Colls.fromItems(Colls.fromArray((1 to 100).map(_.toByte).toArray))
    val longs = (1 to 100).map(_.toLong).toArray

    object ByteArrayArrayConstant {
      def apply(value: Coll[Coll[Byte]]): CollectionConstant[SByteArray.type] = CollectionConstant[SByteArray.type](value, SByteArray)
    }


    val output = ErgoBox(1, alicePubKey, 10, Nil,
      Map(
        R4 -> ByteConstant(1),
        R5 -> SigmaPropConstant(alicePubKey),
        R6 -> IntConstant(10),
        R7 -> LongArrayConstant(longs),
        R8 -> ByteArrayConstant(Base16.decode("123456123456123456123456123456123456123456123456123456123456123456").get),
        R9 -> ByteArrayArrayConstant(collCollByte)
      )
    )

    val input = ErgoBox(1, spamScript, 10, Nil,
      Map(
        R4 -> ByteConstant(1),
        R5 -> SigmaPropConstant(alicePubKey),
        R6 -> IntConstant(10),
        R7 -> LongArrayConstant(longs),
        R8 -> ByteArrayConstant(Base16.decode("123456123456123456123456123456123456123456123456123456123456123456").get),
        R9 -> ByteArrayArrayConstant(collCollByte)
      )
    )
    val outBoxes: IndexedSeq[ErgoBoxCandidate] = IndexedSeq.fill(minNumOutputs)(output)
    val inBoxes: IndexedSeq[ErgoBox] = IndexedSeq.fill(minNumOutputs)(input)
    //normally this transaction would invalid (why?), but we're not checking it in this test
    val tx = createTransaction(outBoxes)

    val context = ErgoLikeContext(
      currentHeight = 10,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = inBoxes,
      spendingTransaction = tx,
      self = inBoxes(0) // what is the use of self?
    )


    val prover = new ContextEnrichingTestProvingInterpreter()

    val pr = prover.withSecrets(alice.dlogSecrets).prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, context, fakeMessage).get

    val verifier = new ErgoLikeTestInterpreter
    val (res, terminated) = termination(() =>
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, context, pr, fakeMessage)
    )
    res.isFailure shouldBe true
    terminated shouldBe true
  }

  property("huge byte array") {
    //TODO coverage: make value dependent on CostTable constants, not magic constant
    val ba = Random.randomBytes(10000000)

    val id = 11: Byte
    val id2 = 12: Byte

    val prover = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(id, ByteArrayConstant(ba))
      .withContextExtender(id2, ByteArrayConstant(ba))

    val spamScript = EQ(CalcBlake2b256(GetVarByteArray(id).get), CalcBlake2b256(GetVarByteArray(id2).get)).toSigmaProp

    val ctx = ErgoLikeContext.dummy(fakeSelf)

    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, ctx.withCostLimit(CostTable.ScriptLimit * 10), fakeMessage).get

    val verifier = new ErgoLikeTestInterpreter
    val (res, terminated) = termination(() =>
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, ctx, pr, fakeMessage)
    )

    res.isFailure shouldBe true
    terminated shouldBe true
  }

  /** This case verifies behavior of script interpreter when given enormously deep tree.
    * Below it is at least 150 levels.
    * When transaction is validated the script is deserialized for execution.
    * It should be checked by deserializer for it's depth.
    * The scripts with more than 150 levels are considered malicious.
    */
  property("big byte array with a lot of operations") {

    val ba = Random.randomBytes(5000000)

    val id = 21: Byte

    val prover = new ContextEnrichingTestProvingInterpreter().withContextExtender(id, ByteArrayConstant(ba))

    val bigSubScript = (1 to 100).foldLeft(CalcBlake2b256(GetVarByteArray(id).get)) { case (script, _) =>
      CalcBlake2b256(script)
    }

    val spamScript = NEQ(bigSubScript, CalcBlake2b256(ByteArrayConstant(Array.fill(32)(0: Byte)))).toSigmaProp

    val ctx = ErgoLikeContext.dummy(fakeSelf).withCostLimit(CostTable.ScriptLimit * 10)

    val prt = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, ctx, fakeMessage)
    prt.isSuccess shouldBe true

    val pr = prt.get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeTestInterpreter
    val (_, terminated) = termination(() =>
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, ctxv, pr.proof, fakeMessage)
    )
    terminated shouldBe true
  }

  property("ring signature - maximum ok ring size") {
    val prover = new ContextEnrichingTestProvingInterpreter()
    val verifier = new ErgoLikeTestInterpreter
    val secret = prover.dlogSecrets.head

    val simulated = (1 to 98).map { _ =>
      new ContextEnrichingTestProvingInterpreter().dlogSecrets.head.publicImage
    }

    val ctx = ErgoLikeContext.dummy(fakeSelf).withCostLimit(CostTable.ScriptLimit * 2)

    val publicImages = secret.publicImage +: simulated
    val prop = OR(publicImages.map(image => SigmaPropConstant(image).isProven)).toSigmaProp

    val proof = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).get

    val (_, terminated) = termination(() =>
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, proof, fakeMessage)
    )
    terminated shouldBe true
  }

  property("transaction with many outputs") {
    forAll(Gen.choose(10, 200), Gen.choose(200, 5000)) { case (orCnt, outCnt) =>
      whenever(orCnt > 10 && outCnt > 200) {
        val orCnt = 10
        val outCnt = 5
        val prover = new ContextEnrichingTestProvingInterpreter()

        val propToCompare = OR((1 to orCnt).map(_ => EQ(LongConstant(6), LongConstant(5)))).toSigmaProp

        val spamProp = OR((1 until orCnt).map(_ => EQ(LongConstant(6), LongConstant(5))) :+
          EQ(LongConstant(6), LongConstant(6))).toSigmaProp

        val spamScript =
          Exists(Outputs,
            FuncValue(Vector((1, SBox)),
              AND(
                GE(ExtractAmount(ValUse(1, SBox)), LongConstant(10)),
                EQ(
                  ExtractScriptBytes(ValUse(1, SBox)),
                  ByteArrayConstant(propToCompare.treeWithSegregation.bytes))
              )
            )
          ).toSigmaProp

        val txOutputs = ((1 to outCnt) map (_ => ErgoBox(11, spamProp, 0))) :+ ErgoBox(11, propToCompare, 0)
        val tx = createTransaction(txOutputs)

        val ctx = ErgoLikeContext.dummy(createBox(0, propToCompare))
          .withTransaction(tx)
          .withCostLimit(CostTable.ScriptLimit * 1000000L)

        val pt0 = System.currentTimeMillis()
        val proof = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, ctx, fakeMessage).get
        val pt = System.currentTimeMillis()
        println(s"Prover time: ${(pt - pt0) / 1000.0} seconds")

        val verifier = new ErgoLikeTestInterpreter
        val (_, terminated) = termination(() =>
          verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, ctx, proof, fakeMessage))
        terminated shouldBe true
      }
    }
  }

  def printTime(name: String, t: Long) = {
    println(s"$name: ${t / 1000.0} seconds")
  }

  def runSpam(name: String, ctx: ErgoLikeContext, prop: ErgoTree, genProofs: Boolean = false, doUnlimitedRun: Boolean = false)
             (prover: ErgoLikeTestProvingInterpreter, verifier: ErgoLikeTestInterpreter): Unit = {
    printTime("Timeout", Timeout)

    val ctxWithoutLimit = ctx.withCostLimit(Long.MaxValue)
    val pr = if (!genProofs) {
      // do not spend time to create a proof
      CostedProverResult(Array[Byte](), ContextExtension.empty, 0L)
    } else {
      val (pr, proveTime) = measureTime(
        prover.prove(emptyEnv + (ScriptNameProp -> (name + "_prove")), prop, ctxWithoutLimit, fakeMessage).get
      )
      printTime("Proof Generation Time", proveTime)
      pr
    }

    // check that execution terminated within timeout due to costing exception and cost limit
    val (res, verifyTime) = measureTime(
      verifier.verify(emptyEnv + (ScriptNameProp -> (name + "_reject_verify")), prop, ctx, pr, fakeMessage)
    )
    printTime("Verifier reject time", verifyTime)

    assert(verifyTime < Timeout, s"Script rejection time $verifyTime is longer than timeout $Timeout")

    assertExceptionThrown(
      res.fold(t => throw t, identity),
      {
        case se: verifier.IR.StagingException =>
          val cause = rootCause(se)
          println(s"Rejection cause: $cause")
          cause.isInstanceOf[CosterException] && cause.getMessage.contains("Estimated expression complexity")
        case _ => false
      }
    )

    // measure time required to fully execute the script itself and check it is more then Timeout
    // this is necessary to nurture a more realistic suite of test cases
    if (doUnlimitedRun) {
      val (_, calcTime) = measureTime {
        verifier.verify(emptyEnv + (ScriptNameProp -> (name + "_full_verify")), prop, ctxWithoutLimit, pr, fakeMessage)
      }
      printTime("Full time to verify", calcTime)
      if (calcTime < Timeout)
        println(s"Script full execution time $calcTime is less than timeout $Timeout")
    }
  }

  property("transaction with many inputs and outputs") {
    implicit lazy val IR = new TestingIRContext {
      override val okPrintEvaluatedEntries = false
    }
    val prover = new ContextEnrichingTestProvingInterpreter()
    val verifier = new ContextEnrichingTestProvingInterpreter()

    val prop = Exists(Inputs,
      FuncValue(Vector((1, SBox)),
        Exists(Outputs,
          FuncValue(Vector((2, SBox)),
            EQ(ExtractScriptBytes(ValUse(1, SBox)),
              ExtractScriptBytes(ValUse(2, SBox))))))).toSigmaProp

    val inputScript = OR((1 to 200).map(_ => EQ(LongConstant(6), LongConstant(5)))).toSigmaProp
    val outputScript = OR((1 to 200).map(_ => EQ(LongConstant(6), LongConstant(6)))).toSigmaProp

    val inputs = ErgoBox(11, prop, 0) +: // the box we are going to spend
      ((1 to 998) map (_ => ErgoBox(11, inputScript, 0))) :+ // non equal boxes
      ErgoBox(11, outputScript, 0) // the last one is equal to output

    val outputs = (1 to 1000) map (_ => ErgoBox(11, outputScript, 0))

    val tx = createTransaction(outputs)

    val ctx = new ErgoLikeContext(currentHeight = 0,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      dataBoxes = ErgoLikeContext.noBoxes,
      headers = ErgoLikeContext.noHeaders,
      preHeader = ErgoLikeContext.dummyPreHeader,
      boxesToSpend = inputs,
      spendingTransaction = tx,
      self = inputs(0),
      extension = ContextExtension.empty,
      validationSettings = ValidationRules.currentSettings,
      costLimit = CostLimit)

    runSpam("t1", ctx, prop, false, true)(prover, verifier)
  }

  property("too heavy avl tree lookup") {
    val reg1 = ErgoBox.nonMandatoryRegisters.head

    def genKey(str: String): ADKey = ADKey @@ Blake2b256("key: " + str)

    def genValue(str: String): ADValue = ADValue @@ Blake2b256("val: " + str)

    implicit lazy val IR = new TestingIRContext {
      override val okPrintEvaluatedEntries = false
    }
    val prover = new ContextEnrichingTestProvingInterpreter()
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)

    (1 to 100000).foreach { i =>
      avlProver.performOneOperation(Insert(genKey(s"key$i"), genValue(s"value$i")))
    }
    avlProver.generateProof()

    val digest = avlProver.digest

    (1 to 100000).foreach { i =>
      avlProver.performOneOperation(Lookup(genKey(s"key$i")))
    }

    val proof = avlProver.generateProof()

    println("proof size: " + proof.length)

    val treeData = SigmaDsl.avlTree(new AvlTreeData(digest, AvlTreeFlags.ReadOnly, 32, None))

    val key1 = genKey("key1")
    val value1 = genValue("value1")

    val prop = ErgoTree(ErgoTree.DefaultHeader, ErgoTree.EmptyConstants,
      EQ(
        IR.builder.mkMethodCall(
          ExtractRegisterAs[SAvlTree.type](Self, reg1).get,
          SAvlTree.getMethod,
          IndexedSeq(ByteArrayConstant(key1), ByteArrayConstant(proof))).asOption[SByteArray].get,
        ByteArrayConstant(value1)
      ).toSigmaProp
    )

    val newBox1 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = createTransaction(newBoxes)

    val s = ErgoBox(20, ErgoScriptPredef.TrueProp, 0, Seq(), Map(reg1 -> AvlTreeConstant(treeData)))

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(s),
      spendingTransaction,
      self = s)

    //    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx.withCostLimit(Long.MaxValue), fakeMessage).get
    //    println("Cost: " + pr.cost)
    //    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, pr, fakeMessage).isFailure shouldBe true
    runSpam("avltree", ctx, prop)(prover, verifier)
  }

  property("nested loops") {
    val alice = new ContextEnrichingTestProvingInterpreter
    val alicePubKey: ProveDlog = alice.dlogSecrets.head.publicImage
    val largeColl = Colls.fromArray((1 to 50).toArray)
    val env = Map(
      ScriptNameProp -> "nested loops",
      "alice" -> alicePubKey,
      "largeColl" -> largeColl
    )
    val spamScript = compile(env,
      """{
        |  val valid  = largeColl.forall({(i:Int) =>
        |     largeColl.exists({(j:Int) =>
        |       i != j
        |     }
        |     ) &&
        |     largeColl.exists({(j:Int) =>
        |       largeColl.forall({(k:Int) =>
        |         k != i + j
        |       }
        |       ) &&
        |       i != j
        |     }
        |     ) &&
        |     OUTPUTS.exists({(x:Box) =>
        |       x.propositionBytes.size >= i
        |     }
        |     )
        |   }
        |  )
        |  ! valid
        |}
      """.stripMargin).asBoolValue.toSigmaProp

    //todo: make value dependent on CostTable constants, not magic constant
    val ba = Random.randomBytes(10000000)

    val id = 11: Byte
    val id2 = 12: Byte

    val prover = new ContextEnrichingTestProvingInterpreter()
      .withContextExtender(id, ByteArrayConstant(ba))
      .withContextExtender(id2, ByteArrayConstant(ba))

    //val spamScript = EQ(CalcBlake2b256(GetVarByteArray(id).get), CalcBlake2b256(GetVarByteArray(id2).get)).toSigmaProp

    val ctx = ErgoLikeContext.dummy(fakeSelf)

    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, ctx, fakeMessage).get

    val verifier = new ErgoLikeTestInterpreter
    val (_, calcTime) = measureTime {
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, ctx, pr, fakeMessage)
    }
    println(s"calc time: $calcTime millis")
    calcTime < Timeout shouldBe true
  }
}