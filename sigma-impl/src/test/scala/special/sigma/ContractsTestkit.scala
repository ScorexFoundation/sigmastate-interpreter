package special.sigma

import special.collection.{Coll, CollOverArrayBuilder}
import scalan._

import scala.reflect.ClassTag

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
  val SigmaDsl: SigmaDslBuilder = new TestSigmaDslBuilder
  val noRegisters = collection[AnyValue]()
  val noBytes = collection[Byte]()
  val noInputs = Array[Box]()
  val noOutputs = Array[Box]()
  val dummyPubkey: Array[Byte] = Array.fill(32)(0: Byte)
  val emptyAvlTree = new TestAvlTree(noBytes, 0, None, None, None)

  def collection[T:RType](items: T*) = Colls.fromArray(items.toArray)

  def regs(m: Map[Byte, AnyValue]): Coll[AnyValue] = {
    val res = new Array[AnyValue](10)
    for ((id, v) <- m) {
      assert(res(id) == null, s"register $id is defined more then once")
      res(id) = v
    }
    Colls.fromArray(res)
  }

  def contextVars(m: Map[Byte, AnyValue]): Coll[AnyValue] = {
    val maxKey = if (m.keys.isEmpty) 0 else m.keys.max
    val res = new Array[AnyValue](maxKey)
    for ((id, v) <- m) {
      val i = id - 1
      assert(res(i) == null, s"register $id is defined more then once")
      res(i) = v
    }
    Colls.fromArray(res)
  }

  val AliceId = Array[Byte](1) // 0x0001
  def newAliceBox(id: Byte, value: Long, registers: Map[Int, AnyValue] = Map()): Box = new TestBox(
    Colls.fromArray(Array[Byte](0, id)), value,
    Colls.fromArray(AliceId), noBytes, noBytes,
    regs(registers.map { case (k, v) => (k.toByte, v) })
  )

  def newContext(height: Int, self: Box, vars: AnyValue*): TestContext = {
    new TestContext(noInputs, noOutputs, height, self, emptyAvlTree, dummyPubkey, vars.toArray)
  }

  implicit class TestContextOps(ctx: TestContext) {
    def withInputs(inputs: Box*) =
      new TestContext(inputs.toArray, ctx.outputs, ctx.height, ctx.selfBox, emptyAvlTree, dummyPubkey, ctx.vars)
    def withOutputs(outputs: Box*) =
      new TestContext(ctx.inputs, outputs.toArray, ctx.height, ctx.selfBox, emptyAvlTree, dummyPubkey, ctx.vars)
    def withVariables(vars: Map[Int, AnyValue]) =
      new TestContext(ctx.inputs, ctx.outputs, ctx.height, ctx.selfBox, emptyAvlTree, dummyPubkey,
        contextVars(vars.map { case (k, v) => (k.toByte, v) }).toArray)
  }

  case class NoEnvContract(condition: Context => Boolean) extends SigmaContract {
    override def builder: SigmaDslBuilder = new TestSigmaDslBuilder
    override def canOpen(ctx: Context): Boolean = condition(ctx)
  }
}
