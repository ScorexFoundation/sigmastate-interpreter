package sigmastate.serialization

import sigmastate.Values.IntConstant
import sigmastate._

class DeserializationResilienceJvm extends DeserializationResilienceTesting {

  implicit lazy val IR: TestingIRContext = new TestingIRContext {
    //    substFromCostTable = false
    saveGraphsInFile = false
    //    override val okPrintEvaluatedEntries = true
  }

  property("reader.level correspondence to the serializer recursive call depth") {
    forAll(logicalExprTreeNodeGen(Seq(AND.apply, OR.apply))) { expr =>
      val (callDepths, levels) = traceReaderCallDepth(expr)
      callDepths shouldEqual levels
    }
    forAll(numExprTreeNodeGen) { numExpr =>
      val expr = EQ(numExpr, IntConstant(1))
      val (callDepths, levels) = traceReaderCallDepth(expr)
      callDepths shouldEqual levels
    }
    forAll(sigmaBooleanGen) { sigmaBool =>
      val (callDepths, levels) = traceReaderCallDepth(sigmaBool)
      callDepths shouldEqual levels
    }
  }


}
