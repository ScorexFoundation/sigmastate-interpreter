package sigmastate.lang

import fastparse.Parsed
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sigmastate.Values._
import sigmastate._

class ContractSignatureParserTest extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {
  def parse(x: String): ContractSignature = {
    ContractSignatureParser.parse(x) match {
      case Parsed.Success(v, _) => v
      case Parsed.Failure(label, index, extra) =>
        fail(s"Parsing failed at index $index: $label with trace: ${extra.trace()}")
    }
  }

  property("parses contract template signature") {
    val source = "@contract def contractName(p1: Int = 5, p2: String = \"default string\", param3: Long)"
    val parsed = fastparse.parse(source, ContractSignatureParser.parse(_)) match {
      case Parsed.Success(v, _) => v
      case Parsed.Failure(label, index, extra) =>
        fail(s"Parsing failed at index $index: $label with trace: ${extra.trace()}")
    }

    parsed.name shouldBe "contractName"
    parsed.params should contain theSameElementsInOrderAs Seq(
      ContractParam("p1", SInt, Some(IntConstant(5))),
      ContractParam("p2", SString, Some(StringConstant("default string"))),
      ContractParam("param3", SLong, None)
    )
  }
}
