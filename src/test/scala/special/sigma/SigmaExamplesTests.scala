package special.sigma

import org.scalatest.FunSuite
import special.sigma.Extensions._

class SigmaExamplesTests extends FunSuite with ContractsTestkit {

  val backer = MockProveDlog(true, noBytes)
  val project = MockProveDlog(true, noBytes)
  val selfId = collection[Byte](0, 1)
  val outId = collection[Byte](0, 2)

  test("crowd funding") {
    val timeout = 100
    val minToRaise = 1000
    val contract = new CrowdFundingContract(timeout, minToRaise, backer, project)
    val self = new TestBox(selfId, 10, noBytes, noBytes, noBytes, noRegisters)

    { // when backer can open
      val ctxForBacker = testContext(noInputs, noOutputs, height = 200, self, emptyAvlTree, dummyPubkey, Array())
      val ok = contract.canOpen(ctxForBacker)
      assert(ok)
    }

    { // then project can open
      val out = new TestBox(outId, minToRaise, noBytes, noBytes, project.propBytes, noRegisters)
      val ctxForProject = testContext(Array(), Array(out), height = 50, self, emptyAvlTree, dummyPubkey, Array())
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
    val out = new TestBox(outId, outValue, noBytes, noBytes, prop, regs(Map(R4 -> toAnyValue(curHeight))))

    { //case 1: demurrage time hasn't come yet
      val ctxForProject = testContext(
        inputs = Array(),
        outputs = Array(out),
        height = outHeight + demurragePeriod - 1,
        self = new TestBox(
          selfId, outValue,
          noBytes, noBytes,
          prop,
          regs(Map(R4 -> toAnyValue(outHeight)))),
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
      val ctxForProject = testContext(
        inputs = Array(),
        outputs = Array(out),
        height = outHeight + demurragePeriod,
        self = new TestBox(
          selfId, outValue,
          noBytes, noBytes,
          prop,
          regs(Map(R4 -> toAnyValue(outHeight)))),
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
        prop, regs(Map(R4 -> toAnyValue(curHeight))))
      val ctxForMiner = testContext(
        inputs = Array(),
        outputs = Array(minerOut),
        height = outHeight + demurragePeriod,
        self = new TestBox(
          selfId, outValue,
          noBytes, noBytes,
          prop,
          regs(Map(R4 -> toAnyValue(outHeight)))),
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
