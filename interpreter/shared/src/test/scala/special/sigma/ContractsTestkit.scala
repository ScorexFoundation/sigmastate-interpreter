package sigma

import sigma.Extensions.ArrayOps
import sigma.ast.ErgoTree
import sigma.ast.syntax.TrueSigmaProp
import sigma.data.{AvlTreeData, RType}
import sigmastate.eval._
import sigmastate.helpers.TestingHelpers._
import sigma.data._

import scala.annotation.nowarn  // imports implicit ClassTag

trait ContractsTestkit {
  val R0 = 0.toByte;
  val R1 = 1.toByte;
  val R2 = 2.toByte;
  val R3 = 3.toByte;
  val R4 = 4.toByte;
  val R5 = 5.toByte;
  val R6 = 6.toByte;
  val R7 = 7.toByte;
  val R8 = 8.toByte;
  val R9 = 9.toByte;
  val Colls = new CollOverArrayBuilder
  val SigmaDsl: SigmaDslBuilder = CSigmaDslBuilder
  val noRegisters = collection[AnyValue]()
  val noBytes = collection[Byte]()
  val noInputs = Array[Box]()
  val noOutputs = Array[Box]()
  val dummyPubkey: Array[Byte] = Array.fill(32)(0: Byte)
  val dummyADDigest: Coll[Byte] = Colls.fromArray(Array.fill(33)(0: Byte))
  val emptyAvlTree = new CAvlTree(AvlTreeData.dummy)
  val noHeaders = CSigmaDslBuilder.Colls.emptyColl[Header]
  val dummyPreHeader: PreHeader = null

  /** Create collection from array of items */
  def collection[T: RType](items: T*) = Colls.fromArray(items.toArray)

  /** Converts a map of registers to collection of registers. */
  def regs(m: Map[Byte, AnyValue]): Coll[AnyValue] = {
    val res = new Array[AnyValue](10)
    for ( (id, v) <- m ) {
      assert(res(id) == null, s"register $id is defined more then once")
      res(id) = v
    }
    Colls.fromArray(res)
  }

  val AliceId = Array[Byte](1) // 0x0001

  def newAliceBox(@nowarn id: Byte, value: Long): Box = {
    val ergoBox = testBox(value,
      ErgoTree.fromProposition(TrueSigmaProp),
      creationHeight = 0, additionalTokens = Seq(), additionalRegisters = Map())
    new CBox(ergoBox)
  }

  def testContext(
      inputs: Array[Box], outputs: Array[Box], height: Int, self: Box,
      tree: AvlTree, minerPk: Array[Byte], activatedScriptVersion: Byte,
      currErgoTreeVersion: Byte, vars: ContextVarsMap) =
    new CContext(
      noInputs.toColl, noHeaders, dummyPreHeader,
      inputs.toColl, outputs.toColl, height, self, inputs.indexOf(self), tree,
      minerPk.toColl, vars, activatedScriptVersion, currErgoTreeVersion)

  def newContext(
      height: Int,
      self: Box,
      activatedScriptVersion: Byte,
      currErgoTreeVersion: Byte,
      vars: ContextVarsMap): CContext = {
    testContext(
      noInputs, noOutputs, height, self, emptyAvlTree, dummyPubkey,
      activatedScriptVersion, currErgoTreeVersion, vars)
  }

  implicit class TestContextOps(ctx: CContext) {
    def withInputs(inputs: Box*) = ctx.copy(inputs = inputs.toArray.toColl)

    def withOutputs(outputs: Box*) = ctx.copy(outputs = outputs.toArray.toColl)
  }
}
