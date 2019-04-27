package sigmastate.utxo.examples

import org.ergoplatform.ErgoBox.R4
import org.ergoplatform.dsl.{ContractSpec, SigmaContractSyntax, StdContracts, TestContractSpec}
import sigmastate.helpers.SigmaTestingCommons
import special.collection.Coll
import special.sigma.{Context,SigmaProp}
import sigmastate.eval.Extensions

class RevenueSharingExamplesSpecification extends SigmaTestingCommons { suite =>
  implicit lazy val IR = new TestingIRContext

  class OutputContract[Spec <: ContractSpec](
  )(implicit val spec: Spec) extends SigmaContractSyntax with StdContracts {

    import syntax._

    lazy val contractEnv = Env()

    lazy val prop = proposition("outputContract", { CONTEXT: Context =>
      import CONTEXT._
      val spenders: Coll[(SigmaProp, Int)] = SELF.R4[Coll[(SigmaProp, Int)]].get
      val index = getVar[Int](1).get
      val spender:(SigmaProp, Int) = spenders(index)
      val pubKey:SigmaProp = spender._1
      val ratio:Int = spender._2
      // val total = spenders.foldLeft(0, {(accum:Int, s:(SigmaProp, Int)) => accum + s._2})
      val total = spenders.foldLeft(0, {accum:(Int, (SigmaProp, Int)) => accum._1+accum._2._2})
      val balance = SELF.value - SELF.value / total * ratio

      val remainingSpenders = spenders.filter(_._1 != pubKey)

      val out = OUTPUTS(0)
      val outSpenders = out.R4[Coll[(SigmaProp, Int)]].get

      val validOut = out.propositionBytes == SELF.propositionBytes &&
                     out.value >= balance && remainingSpenders == outSpenders
      pubKey && (outSpenders.size == 0 || validOut)
    },
    """{
      |      val spenders: Coll[(SigmaProp, Int)] = SELF.R4[Coll[(SigmaProp, Int)]].get
      |      val index = getVar[Int](1).get
      |      val spender:(SigmaProp, Int) = spenders(index)
      |      val pubKey:SigmaProp = spender._1
      |      val ratio:Int = spender._2
      |      // below syntax can be simplified but does not work
      |      val total = spenders.fold(0, {(accum:Int, s:(SigmaProp, Int)) => accum + s._2})
      |      //val total = spenders.foldLeft(0, {accum:(Int, (SigmaProp, Int)) => accum._1+accum._2._2})
      |      val balance = SELF.value - SELF.value / total * ratio
      |
      |      val getNew = {(s:(SigmaProp, Int)) => if (s._1 == pubKey) (s._1, 0) else (s._1, s._2)}
      |
      |      val out = OUTPUTS(0)
      |
      |      //val leftSpenders = spenders.map({(s:(SigmaProp, Int)) => getNew(s)})
      |      val leftSpenders = spenders
      |      val validNextSpenders = out.R4[Coll[(SigmaProp, Int)]].get == leftSpenders
      |
      |      val validOut = out.propositionBytes == SELF.propositionBytes &&
      |                     out.value >= balance && validNextSpenders
      |
      |      pubKey && (total == ratio || validOut)
      |}
    """.stripMargin)
  }

  case class RevenueContract[Spec <: ContractSpec]
      (alice: Spec#ProvingParty, bob: Spec#ProvingParty, carol:Spec#ProvingParty)
      (implicit val spec: Spec) extends SigmaContractSyntax with StdContracts
  {
    import syntax._
    val pkDummy = alice.pubKey

    val spenders = Coll((alice.pubKey, 50), (bob.pubKey, 30), (carol.pubKey, 20))

    val outputContract = new OutputContract()

    val outPropBytes:Coll[Byte] = Coll(outputContract.prop.ergoTree.bytes:_*)

    val outPropBytesHash:Coll[Byte] = blake2b256(outPropBytes)

    lazy val contractEnv = Env("pkDummy" -> pkDummy, "spenders" -> spenders, "outPropBytesHash" -> outPropBytesHash)

    lazy val prop = proposition("revenueContract", { CONTEXT: Context =>
      import CONTEXT._
      val index = getVar[Int](1).get
      val spender = spenders(index)
      val pubKey:SigmaProp = spender._1
      val ratio = spender._2
      val balance = SELF.value - SELF.value / 100 * ratio
      val remainingSpenders = spenders.filter(_._1 != pubKey)
      val validOut = blake2b256(OUTPUTS(0).propositionBytes) == outPropBytesHash &&
                     OUTPUTS(0).value >= balance &&
                     OUTPUTS(0).R4[Coll[(SigmaProp, Int)]].get == remainingSpenders
      pubKey && validOut
    },
    """{
      |      pkDummy
      |}
    """.stripMargin)

    lazy val requireDummySignature  = proposition("dummySignature", _ => pkDummy, "pkDummy")
  }

  lazy val spec = TestContractSpec(suite)(new TestingIRContext)

  lazy val alice = spec.ProvingParty("Alice")
  lazy val bob = spec.ProvingParty("Bob")
  lazy val carol = spec.ProvingParty("Carol")

  ignore("Revenue sharing contract") {
    val contract = RevenueContract[spec.type](alice, bob, carol)(spec)

    import contract.spec._

    val mockTx = candidateBlock(0).newTransaction()

    val deposit = mockTx.outBox(100, contract.prop)

    val tx = candidateBlock(10).newTransaction().spending(deposit)

    tx.outBox(70, contract.outputContract.prop).withRegs(
      R4 -> Coll[(SigmaProp, Int)](
        Array((alice.pubKey, 50), (carol.pubKey, 20))
      )
    )

    tx.outBox(value = 30, contract.requireDummySignature)

    val in = tx.inputs(0)

    val res = in.runDsl(Map(1.toByte -> Extensions.toAnyValue(1)))

    val pr = alice.prove(in).get
    contract.verifier.verify(in, pr) shouldBe true
  }
}
