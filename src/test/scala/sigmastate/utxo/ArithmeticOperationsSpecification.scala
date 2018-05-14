package sigmastate.utxo

import sigmastate.Values._
import sigmastate._
import sigmastate.helpers.{ErgoProvingInterpreter, SigmaTestingCommons}

class ArithmeticOperationsSpecification extends SigmaTestingCommons {

  property("Plus Minus Multiply") {
    val elementId = 1.toByte
    val prover = (new ErgoProvingInterpreter).withContextExtender(elementId, IntConstant(1))

    val prop = EQ(Minus(10, TaggedInt(elementId)), Plus(3, Multiply(2, 3)))
    /*
    val env = Map("elementId" -> elementId.toLong)
    val compiledScript = compile(env,
      """{
        |  10 + taggedInt(elementId) == 4 + 2 * 3
        |}
      """.stripMargin)
    compiledScript shouldBe prop
    */

    val ctx = ErgoContext.dummy(fakeSelf)
    val pr = prover.prove(prop, ctx, fakeMessage).get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoInterpreter
    verifier.verify(prop, ctx, pr.proof, fakeMessage).map(_._1).getOrElse(false) shouldBe false //context w/out extensions
    verifier.verify(prop, ctxv, pr.proof, fakeMessage).get._1 shouldBe true
  }

}
