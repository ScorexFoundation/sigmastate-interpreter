package special.sigma

import org.bouncycastle.math.ec.custom.sec.SecP256K1Point
import org.scalatest.FunSuite

class SigmaExamplesTests extends FunSuite with ContractsTestkit {

  val backer = new ProveDlogEvidence(SigmaDsl.groupGenerator.twice().asInstanceOf[SecP256K1Point])
  val project = new ProveDlogEvidence(SigmaDsl.groupGenerator.threeTimes().asInstanceOf[SecP256K1Point])
  val selfId = collection[Byte](0, 1)
  val outId = collection[Byte](0, 2)

  test("crowd funding") {
    val timeout = 100
    val minToRaise = 1000
    val contract = new CrowdFundingContract(timeout, minToRaise, backer, project)
    val bytes = Colls.fromArray(Array[Byte]())
    val self = new TestBox(selfId, 10, noBytes, noBytes, noBytes, noRegisters)

    { // when backer can open
      val ctxForBacker = new TestContext(noInputs, noOutputs, height = 200, self, emptyAvlTree, dummyPubkey, Array())
      val ok = contract.canOpen(ctxForBacker)
      assert(ok)
      assert(self.dataSize == noBytes.length)
    }

    { // then project can open
      val out = new TestBox(outId, minToRaise, noBytes, noBytes, project.propBytes, noRegisters)
      val ctxForProject = new TestContext(Array(), Array(out), height = 50, self, emptyAvlTree, dummyPubkey, Array())
      val ok = contract.canOpen(ctxForProject)
      assert(ok)
    }
  }

  test("demurrage") {
    val demurragePeriod = 100
    val demurrageCost = 2
    val userProof = new MockProveDlog(isValid = true, noBytes)
    val contract = new DemurrageCurrencyContract(demurragePeriod, demurrageCost, userProof)

    val prop = Colls.fromArray(Array[Byte](1, 2))
    val outHeight = 100
    val outValue = 10L
    val curHeight = outHeight + demurragePeriod
    val out = new TestBox(outId, outValue, noBytes, noBytes, prop, regs(Map(R4 -> curHeight)))

    { //case 1: demurrage time hasn't come yet
      val ctxForProject = new TestContext(
        inputs = Array(),
        outputs = Array(out),
        height = outHeight + demurragePeriod - 1,
        selfBox = new TestBox(
          selfId, outValue,
          noBytes, noBytes,
          prop,
          regs(Map(R4 -> outHeight))),
        emptyAvlTree,
        dummyPubkey,
        vars = Array()
      )
      userProof.isValid = true
      val userCan = contract.canOpen(ctxForProject)
      assert(userCan)

      userProof.isValid = false
      val minerCan = contract.canOpen(ctxForProject)
      assert(!minerCan)
    }

    { //case 2: demurrage time has come (user can spend all the money)
      val ctxForProject = new TestContext(
        inputs = Array(),
        outputs = Array(out),
        height = outHeight + demurragePeriod,
        selfBox = new TestBox(
          selfId, outValue,
          noBytes, noBytes,
          prop,
          regs(Map(R4 -> outHeight))),
        emptyAvlTree,
        dummyPubkey,
        vars = Array()
      )
      userProof.isValid = true
      val userCan = contract.canOpen(ctxForProject)
      assert(userCan)
    }

    { //case 3: demurrage time has come (miner can spend "demurrageCost" tokens)
      val minerOut = new TestBox(outId, outValue - demurrageCost,
        noBytes, noBytes,
        prop, regs(Map(R4 -> curHeight)))
      val ctxForMiner = new TestContext(
        inputs = Array(),
        outputs = Array(minerOut),
        height = outHeight + demurragePeriod,
        selfBox = new TestBox(
          selfId, outValue,
          noBytes, noBytes,
          prop,
          regs(Map(R4 -> outHeight))),
        emptyAvlTree,
        dummyPubkey,
        vars = Array()
      )
      userProof.isValid = false
      val minerCan = contract.canOpen(ctxForMiner)
      assert(minerCan)
    }
  }
}
