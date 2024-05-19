package sigma

import sigma.ast.{FuncValue, Global, MethodCall, SByte, SCollection, SGlobalMethods, STypeVar, ValUse, Value}
import sigma.data.RType
import sigma.eval.SigmaDsl

import scala.util.Success

/** This suite tests all operations for v6.0 version of the language.
  * The base classes establish the infrastructure for the tests.
  *
  * @see SigmaDslSpecificationBase
  */
class LanguageSpecificationV6 extends LanguageSpecificationBase { suite =>
  override def languageVersion: Byte = VersionContext.V6SoftForkVersion

  import TestData._

  def mkSerializeFeature[A: RType]: Feature[A, Coll[Byte]] = {
    val tA = RType[A]
    val tpe = Evaluation.rtypeToSType(tA)
    newFeature(
      (x: A) => SigmaDsl.serialize(x),
      s"{ (x: ${tA.name}) => serialize(x) }",
      expectedExpr = FuncValue(
        Array((1, tpe)),
        MethodCall(
          Global,
          SGlobalMethods.serializeMethod.withConcreteTypes(Map(STypeVar("T") -> tpe)),
          Array(ValUse(1, tpe)),
          Map()
        )
      ),
      sinceVersion = VersionContext.V6SoftForkVersion)
  }

  property("Global.serialize[Byte]") {
    lazy val serializeByte = mkSerializeFeature[Byte]
    val cases = Seq(
      (-128.toByte, Success(Coll(-128.toByte))),
      (-1.toByte, Success(Coll(-1.toByte))),
      (0.toByte, Success(Coll(0.toByte))),
      (1.toByte, Success(Coll(1.toByte))),
      (127.toByte, Success(Coll(127.toByte)))
    )
    testCases(cases, serializeByte)
  }

  property("Global.serialize[Short]") {
    lazy val serializeShort = mkSerializeFeature[Short]
    val cases = Seq(
      (Short.MinValue, Success(Coll[Byte](0xFF.toByte, 0xFF.toByte, 0x03.toByte))),
      (-1.toShort, Success(Coll(1.toByte))),
      (0.toShort, Success(Coll(0.toByte))),
      (1.toShort, Success(Coll(2.toByte))),
      (Short.MaxValue, Success(Coll(-2.toByte, -1.toByte, 3.toByte)))
    )
    testCases(cases, serializeShort)
  }
}
