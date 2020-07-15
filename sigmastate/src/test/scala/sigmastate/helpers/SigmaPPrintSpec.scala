package sigmastate.helpers

import org.ergoplatform.{Outputs, ErgoBox}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{PropSpec, Matchers}
import sigmastate.Values._
import sigmastate.lang.Terms.MethodCall
import sigmastate.serialization.OpCodes
import sigmastate.utxo.SelectField
import sigmastate._

import scala.collection.mutable.ArrayBuffer

class SigmaPPrintSpec extends PropSpec
    with PropertyChecks
    with Matchers {


  property("typeName") {
    def test(t: SType, exp: String) = {
      SigmaPPrint.typeName(t) shouldBe exp
    }
    test(SInt, "SInt.type")
    test(SCollection(SBox), "SCollection[SBox.type]")
    test(SOption(SBox), "SOption[SBox.type]")
    test(STuple(SBox, SInt), "STuple")
  }

  property("Special cases") {
    def test(x: Any, expected: String) = {
      SigmaPPrint(x).plainText shouldBe expected
    }

    // type handlers
    test(SCollectionType(SByte), "SByteArray")
    test(SCollectionType(SCollectionType(SByte)), "SByteArray2")
    test(SCollectionType(SBoolean), "SBooleanArray")
    test(STuple(Vector(SBoolean, SInt)), "SPair(SBoolean, SInt)")

    // exception handlers
    test(new ArithmeticException("msg"), "new ArithmeticException(\"msg\")")

    // additionalHandlers
    test(SGlobal, "SGlobal")
    test(SCollection, "SCollection")
    test(SInt, "SInt")
    test(Outputs, "Outputs")
    test(10.toByte, "10.toByte")
    test(255.toByte, "-1.toByte")
    test(Array(10): Seq[Int], "Array(10)")
    test({val buf = ArrayBuffer.empty[Int]; buf += (10); buf}, "Seq(10)")
    test(ErgoBox.R0, "ErgoBox.R0")
    test(ErgoBox.R9, "ErgoBox.R9")
    test(
      SelectField.typed[Value[SByte.type]](ValUse(1, STuple(Vector(SByte, SByte))), 1.toByte),
      "SelectField.typed[Value[SByte.type]](ValUse(1, SPair(SByte, SByte)), 1.toByte)"
    )
    test(IntConstant(10), "IntConstant(10)")
    test(ArithOp(IntConstant(1), IntConstant(1), OpCodes.PlusCode), "ArithOp(IntConstant(1), IntConstant(1), OpCode @@ (-102.toByte))")
    test(
      MethodCall.typed[Value[SCollection[SBox.type]]](
        ValUse(1, SContext),
        SContext.getMethodByName("dataInputs"),
        Vector(),
        Map()
      ),
      """MethodCall.typed[Value[SCollection[SBox.type]]](
        |  ValUse(1, SContext),
        |  SContext.getMethodByName("dataInputs"),
        |  Vector(),
        |  Map()
        |)""".stripMargin)

    test(SCollection.tIV, """STypeVar("IV")""")
    test(Map(SCollection.tIV -> SInt), """Map(STypeVar("IV") -> SInt)""")
    test(
      MethodCall.typed[Value[SCollection[SInt.type]]](
        ValUse(1, SCollectionType(SBox)),
        SCollection.IndicesMethod.withConcreteTypes(Map(SCollection.tIV -> SBox)),
        Vector(),
        Map()
      ),
      """MethodCall.typed[Value[SCollection[SInt.type]]](
        |  ValUse(1, SCollectionType(SBox)),
        |  SCollection.getMethodByName("indices").withConcreteTypes(Map(STypeVar("IV") -> SBox)),
        |  Vector(),
        |  Map()
        |)""".stripMargin)
  }
}
