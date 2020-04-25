package special.collections

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ExamplesTests extends PropSpec with PropertyChecks with Matchers with CollGens with ExampleGens { testSuite =>
  import Examples._
  val examples = new Examples(builder)
  import examples._

  property("checkTokenBalance") {
    forAll(genContext) { ctx =>
      checkTokenBalance(ctx)
    }
  }

  property("tokenTotal") {
    forAll(genContext) { ctx =>
      val tokenId = ctx.inputs(0).tokens(0)._1
      val t1 = tokenTotal1(ctx, tokenId)
      val t2 = tokenTotal2(ctx, tokenId)
      val t3 = tokenTotal3(ctx, tokenId)
      t1 shouldBe t2
      t2 shouldBe t3
    }
  }
}
