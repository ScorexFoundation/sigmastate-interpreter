package sigmastate.utxo.examples

import org.bouncycastle.jcajce.provider.digest.Blake2b.Blake2b256
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.R4
import org.ergoplatform.dsl.{ContractSpec, SigmaContractSyntax, StdContracts, TestContractSpec}
import scorex.crypto.hash
import scorex.crypto.hash.Blake2b256
import sigmastate.helpers.SigmaTestingCommons
import special.collection.Coll
import special.sigma.{Context, Extensions, SigmaProp}

class DummyExamplesSpecification extends SigmaTestingCommons { suite =>
  implicit lazy val IR = new TestingIRContext

  private val reg1 = ErgoBox.nonMandatoryRegisters(0)

  case class DummyContract[Spec <: ContractSpec]
    (alice: Spec#ProvingParty)(implicit val spec: Spec) extends SigmaContractSyntax with StdContracts {
    def pkA = alice.pubKey
    lazy val contractEnv = Env("pkA" -> pkA)

    lazy val prop = proposition("dummyContract", { CONTEXT: Context =>
      import CONTEXT._
      val oldRoot = SELF.R4[Coll[Byte]].get
      val oldItem: Coll[Byte] = getVar[Coll[Byte]](1).get
      val newItem: Coll[Byte] = getVar[Coll[Byte]](2).get
      val newRoot = OUTPUTS(0).R4[Coll[Byte]].get
      val path = getVar[Coll[Coll[Byte]]](0).get

      def xor(a: (Coll[Byte], Coll[Byte])):Coll[Byte] = {
        a._1.zip(a._2).map({ (c: (Byte, Byte)) => (c._1 ^ c._2).toByte })
      }

      def getRoot(item: Coll[Byte]):Coll[Byte] = path.foldLeft(item, xor)
      pkA && getRoot(oldItem) == oldRoot && getRoot(newItem) == newRoot
    },
      """{
        |      val oldRoot = SELF.R4[Coll[Byte]].get
        |      val oldItem: Coll[Byte] = getVar[Coll[Byte]](1).get
        |      val newItem: Coll[Byte] = getVar[Coll[Byte]](2).get
        |      val newRoot = OUTPUTS(0).R4[Coll[Byte]].get
        |      val path = getVar[Coll[Coll[Byte]]](0).get
        |
        |      val getRoot = {(item: Coll[Byte]) => path.fold(item, {
        |        (a: (Coll[Byte], Coll[Byte])) =>
        |        a._1.zip(a._2).map({ (c: (Byte, Byte)) => (c._1 ^ c._2).toByte })
        |      })}
        |      pkA && getRoot(oldItem) == oldRoot && getRoot(newItem) == newRoot
        |}
      """.stripMargin)

  }
  lazy val spec = TestContractSpec(suite)(new TestingIRContext)
  lazy val alice = spec.ProvingParty("Alice")

  property("Dummy contract") {
    val contract = DummyContract[spec.type](alice)(spec)
    import contract.spec._

    val mockTx = candidateBlock(0).newTransaction()

    val dummyBytes:Coll[Byte] = hash.Blake2b256("".getBytes)
    val dummyPath:Coll[Coll[Byte]] = Array(dummyBytes)
    val deposit = mockTx.outBox(100, contract.prop).withRegs(reg1 -> dummyBytes)

    val tx = candidateBlock(10).newTransaction().spending(deposit)

    tx.outBox(70, contract.prop).withRegs(reg1 -> dummyBytes)

    val in = tx.inputs(0)

    val res = in.runDsl(
      Map(
        0.toByte -> Extensions.toAnyValue(dummyPath),
        1.toByte -> Extensions.toAnyValue(dummyBytes),
        2.toByte -> Extensions.toAnyValue(dummyBytes)
      )
    )

    val pr = alice.prove(in).get
    contract.verifier.verify(in, pr) shouldBe true
  }
}
