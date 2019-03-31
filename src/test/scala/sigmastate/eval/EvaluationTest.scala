package sigmastate.eval

import org.ergoplatform.ErgoBox
import sigmastate.Values.{ConcreteCollection, IntArrayConstant, IntConstant, SigmaPropConstant, SigmaPropValue, Value}
import sigmastate.helpers.ContextEnrichingTestProvingInterpreter
import sigmastate.interpreter.Interpreter._
import scalan.BaseCtxTests
import sigmastate.lang.LangTests
import scalan.util.BenchmarkUtil._
import sigmastate._
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}
import sigmastate.serialization.ErgoTreeSerializer

class EvaluationTest extends BaseCtxTests
    with LangTests with ExampleContracts with ErgoScriptTestkit {

  test("constants") {
    val ctx = newErgoContext(height = 1, boxToSpend)
    reduce(emptyEnv, "one", "1", ctx,  1)
    reduce(emptyEnv, "oneL", "1L", ctx, 1L)
  }

  test("operations") {
    val ctx = newErgoContext(height = 1, boxToSpend)
    reduce(emptyEnv, "one+one", "1 + 1", ctx, 2)
    reduce(emptyEnv, "oneL+oneL", "1L - 1L", ctx, 0L)
    reduce(emptyEnv, "one_gt_one", "1 > 1", ctx, false)
    reduce(emptyEnv, "or", "1 > 1 || 2 < 1", ctx, false)
    reduce(emptyEnv, "or2", "1 > 1 || 2 < 1 || 2 > 1", ctx, true)
    reduce(emptyEnv, "or3", "OUTPUTS.size > 1 || OUTPUTS.size <= 1", ctx, true)
    reduce(emptyEnv, "and", "1 > 1 && 2 < 1", ctx, false)
    reduce(emptyEnv, "and2", "1 > 1 && 2 < 1 && 2 > 1", ctx, false)
    reduce(emptyEnv, "and3", "1 == 1 && (2 < 1 || 2 > 1)", ctx, true)
    reduce(emptyEnv, "and4", "OUTPUTS.size > 1 && OUTPUTS.size <= 1", ctx, false)
  }

  test("lazy logical ops") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val pk = prover.dlogSecrets.head.publicImage
    val self = ErgoBox(1, pk, 0, additionalRegisters = Map(ErgoBox.R4 -> IntConstant(10)))
    val ctx = newErgoContext(height = 1, self)
    // guarded register access: existing reg
    reduce(emptyEnv, "lazy1", "SELF.R4[Int].isDefined && SELF.R4[Int].get == 10", ctx, true)
    // guarded register access: non-existing reg
    reduce(emptyEnv, "lazy2", "SELF.R5[Int].isDefined && SELF.R5[Int].get == 10", ctx, false)

    // guarded register access: reading register if it is defined and another one is undefined
    reduce(emptyEnv, "lazy3", "SELF.R4[Int].isDefined && (SELF.R5[Int].isDefined || SELF.R4[Int].get == 10)", ctx, true)
  }

  test("context data") {
    val ctx = newErgoContext(height = 100, boxToSpend)
        .withTransaction(tx1)
    reduce(emptyEnv, "height1", "HEIGHT + 1L", ctx, 101)
    reduce(emptyEnv, "height2", "HEIGHT > 1L", ctx, true)
    reduce(emptyEnv, "size", "INPUTS.size + OUTPUTS.size", ctx, 3)
    reduce(emptyEnv, "value", "SELF.value + 1L", ctx, 11L)
  }

  // TODO Caused by: java.lang.AssertionError: assertion failed:
  //  Invalid cast Cast(SizeCollElem<SizeColl[Coll[Int], Int]>, s1572): interface special.collection.SizeColl is not assignable from class special.collection.CSizePrim
  test("lambdas") {
    val ctx = newErgoContext(height = 1, boxToSpend)
    reduce(emptyEnv, "lam3", "{ val f = { (out: Box) => out.value >= 0L }; f(SELF) }", ctx, true)

    // access R5 and call g only if f returns false
    reduce(emptyEnv, "lam4",
      """{
       |  def f(out: Box) = out.value >= 0L;
       |  def g(x: Int) = x < 0;
       |  f(SELF) || g(SELF.R5[Int].get)
       | }""".stripMargin, ctx, true)

    reduce(emptyEnv, "lam5",
      """{
       |  val f = { (out: Box) => out.value >= 0L };
       |  val g = { (xs: Coll[Int]) => xs.size > 0 };
       |  f(SELF) || g(SELF.R5[Coll[Int]].get)
       | }""".stripMargin, ctx, true)
  }

  test("Measure IRContext creation speed") {
    var ctx: RuntimeIRContext = new RuntimeIRContext
    measure(100) { i =>
      ctx = new RuntimeIRContext
    }
    println(s"Def count: ${ctx.defCount}")
    /*
    Iter 0: 4 ms
        ...
    Iter 96: 2 ms
    Iter 97: 1 ms
    Iter 98: 2 ms
    Iter 99: 2 ms
    Total time: 244 ms
    Def count: 20
    */
  }
  
//  test("Crowd Funding") {
//    val backerProver = new ErgoLikeProvingInterpreter
//    val projectProver = new ErgoLikeProvingInterpreter
//    val backerPubKey = backerProver.dlogSecrets.head.publicImage
//    val projectPubKey = projectProver.dlogSecrets.head.publicImage
//    val ctxVars = contextVars(Map(
//      backerPubKeyId -> backerPubKey,
//      projectPubKeyId -> projectPubKey
//    )).arr
//    val ctx = newContext(height = 1, boxA1, ctxVars:_*)
//    reduce(envCF, "CrowdFunding", crowdFundingScript, ctx, FalseLeaf)
//
//    val boxToSpend = ErgoBox(10, TrueLeaf)
//    val tx1Output1 = ErgoBox(minToRaise, projectPubKey)
//    val tx1Output2 = ErgoBox(1, projectPubKey)
//    val tx1 = createTransaction(IndexedSeq(tx1Output1, tx1Output2))
//    val ergoCtx = ErgoLikeContext(
//      currentHeight = timeout - 1,
//      lastBlockUtxoRoot = AvlTreeData.dummy,
//      boxesToSpend = IndexedSeq(),
//      spendingTransaction = tx1,
//      self = boxToSpend,
//      extension = ContextExtension(Map(
//        backerPubKeyId -> SigmaPropConstant(backerPubKey),
//        projectPubKeyId -> SigmaPropConstant(projectPubKey)
//      )))
//    reduce(envCF, "CrowdFunding", crowdFundingScript, ergoCtx.toTestContext, projectPubKey)
//  }

  test("SubstConst") {
    def script(pk: ProveDlog): SigmaPropValue =
      AND(EQ(IntConstant(1), IntConstant(1)), SigmaPropConstant(pk).isProven).toSigmaProp

    val pk1 = DLogProverInput.random().publicImage
    val pk2 = DLogProverInput.random().publicImage
    val script1 = script(pk1)
    val script2 = script(pk2)
    val inputBytes = ErgoTreeSerializer.DefaultSerializer.serializeWithSegregation(script1)
    val positions = IntArrayConstant(Array[Int](2))
    // in ergo we have only byte array of a serialized group element
    val newVals = ConcreteCollection(Vector[SigmaPropValue](CreateProveDlog(DecodePoint(pk2.pkBytes))), SSigmaProp)

    val expectedBytes = ErgoTreeSerializer.DefaultSerializer.serializeWithSegregation(script2)
    val ctx = newErgoContext(height = 1, boxToSpend)
    reduce(emptyEnv, "SubstConst",
      EQ(SubstConstants(inputBytes, positions, newVals), expectedBytes),
      ctx,
      true)
  }
}
