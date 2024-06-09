package sigma.serialization

import scorex.utils.Ints
import sigma.VersionContext
import sigma.ast._
import sigma.validation.ValidationException

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

  property("MethodCall deserialization round trip for Global.powHit") {
    val k = IntConstant(32)
    val msg = ByteArrayConstant(Array.fill(5)(1.toByte))
    val nonce = ByteArrayConstant(Array.fill(8)(2.toByte))
    val h = ByteArrayConstant(Ints.toByteArray(5))
    val N = IntConstant(1024 * 1024)

    def code = {
      val bi = BigIntConstant(5)
      val expr = MethodCall(Global,
        SGlobalMethods.powHitMethod,
        Vector(k, msg, nonce, h, N),
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
}
