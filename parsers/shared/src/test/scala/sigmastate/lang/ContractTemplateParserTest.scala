package sigmastate.lang

//import fastparse.Parsed
//import org.scalatest.matchers.should.Matchers
//import org.scalatest.propspec.AnyPropSpec
//import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
//import sigmastate._
//
//class ContractTemplateParserTest extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {
//  def parse(x: String): ParsedContractTemplate = {
//    ContractTemplateParser.parse(x) match {
//      case Parsed.Success(v, _) => v
//      case Parsed.Failure(label, index, extra) =>
//        fail(s"Parsing failed at index $index: $label with trace: ${extra.trace(true)}")
//    }
//  }
//
//  property("parses contract template") {
//    val source =
//      """/** This is my contracts description.
//        |* Here is another line describing what it does in more detail.
//        |*
//        |* @param p1 describe p1
//        |* @param p2 description of the 2nd parameter
//        |* which is pretty complex and on many
//        |* lines to describe functions
//        |* @param p3 the final parameter
//        |* @return
//        |*/
//        |@contract def contractName(p1: Int = 5, p2: String = "default string", param3: Long) {
//        |  sigmaProp(true)
//        |}""".stripMargin
//
//    ContractTemplateParser.parse(source) shouldBe ""
//  }
//}
