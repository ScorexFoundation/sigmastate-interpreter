package sigma.serialization

import sigma.VersionContext
import sigma.ast._
import sigma.validation.ValidationException

import scala.util.Try


class MethodCallSerializerSpecification extends SerializationSpecification {

  property("MethodCall deserialization round trip") {
    val expr = MethodCall(Outputs,
      SCollectionMethods.FlatMapMethod.withConcreteTypes(Map(SCollection.tIV -> SBox, SCollection.tOV -> SByte)),
      Vector(FuncValue(1, SBox, ExtractScriptBytes(ValUse(1, SBox)))),
      Map()
    )
    roundTripTest(expr)
  }

  property("MethodCall deserialization round trip (non-generic method)") {
    val expr = MethodCall(Outputs,
      SCollectionMethods.SizeMethod.withConcreteTypes(Map(SCollection.tIV -> SBox)),
      Vector(),
      Map()
    )
    roundTripTest(expr)
  }

  property("MethodCall deserialization round trip for BigInt.nbits") {
    def code = {
      val bi = BigIntConstant(5)
      val expr = MethodCall(bi,
        SBigIntMethods.ToNBits,
        Vector(),
        Map()
      )
      roundTripTest(expr)
    }

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, 1) {
      code
    }

    an[ValidationException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      }
      )
  }

  property("MethodCall deserialization round trip for Header.checkPow") {
    def code = {
      val h = HeaderConstant(headerGen.sample.get)
      val expr = MethodCall(h,
        SHeaderMethods.checkPowMethod,
        Vector(),
        Map()
      )
      roundTripTest(expr)
    }

    VersionContext.withVersions(VersionContext.V6SoftForkVersion, 1) {
      code
    }

    // sigma.serialization.SerializerException: Don't know how to serialize (sigma.data.CHeader@51dbec76, SHeader)
    an[SerializerException] should be thrownBy (
      VersionContext.withVersions((VersionContext.V6SoftForkVersion - 1).toByte, 1) {
        code
      }
    )
  }

}
