package sigmastate

import special.sigma.SigmaTestingData

import scala.util.{Try, Failure}

class JitCostSpecification extends SigmaTestingData {

  type BinOp = (Int, Int) => Int
  type JitBinOp = (JitCost, JitCost) => JitCost

  def testBinary(x: Int, y: Int, f: BinOp, jitf: JitBinOp) = {
    val r1 = Try(f(x, y))
    val r2 = Try(jitf(new JitCost(x), new JitCost(y)).value)
    (r1, r2) match {
      case (Failure(exception), Failure(expectedException)) =>
        rootCause(exception).getClass shouldBe expectedException.getClass
      case _ =>
        r1 shouldBe r2
    }
  }

  property("JitCost.$plus") {
    forAll(MinSuccessful(1000)) { (x: Int, y: Int) =>
      testBinary(x, y, java7.compat.Math.addExact, _ + _)
    }
  }

  property("JitCost scaling") {
    forAll(MinSuccessful(1000)) { (x: Int) =>
      whenever(0 <= x && x <= 100000000) {
        JitCost.fromBlockCost(x).toBlockCost shouldBe x
      }
    }
  }

}
