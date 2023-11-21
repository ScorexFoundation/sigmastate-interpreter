package sigmastate.lang

import fastparse.Parsed
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ContractDocParserTest extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {
  def parse(x: String): ContractDoc = {
    ContractDocParser.parse(x) match {
      case Parsed.Success(v, _) => v
      case Parsed.Failure(label, index, extra) =>
        fail(s"Parsing failed at index $index: $label with trace: ${extra.trace()}")
    }
  }

  property("parses docstring in example") {
    val doc =
      """/** This is my contracts description.
        |* Here is another line describing what it does in more detail.
        |*
        |* @param p1 describe p1
        |* @param p2 description of the 2nd parameter
        |* which is pretty complex and on many
        |* lines to describe functions
        |* @param p3 the final parameter
        |* @return
        |*/""".stripMargin
    val contractDoc = parse(doc)

    contractDoc.description shouldBe "This is my contracts description. Here is another line describing what it does in more detail."
    contractDoc.params should contain theSameElementsInOrderAs Seq(
      ParameterDoc("p1", "describe p1"),
      ParameterDoc("p2", "description of the 2nd parameter which is pretty complex and on many lines to describe functions"),
      ParameterDoc("p3", "the final parameter")
    )
  }
}
