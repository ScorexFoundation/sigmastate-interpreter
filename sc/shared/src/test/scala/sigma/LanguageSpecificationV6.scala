package sigma

/** This suite tests all operations for v6.0 version of the language.
  * The base classes establish the infrastructure for the tests.
  *
  * @see SigmaDslSpecificationBase
  */
class LanguageSpecificationV6 extends LanguageSpecificationBase { suite =>
  override def languageVersion: Byte = VersionContext.V6SoftForkVersion

  import TestData._

//  property("Global.serialize") {
//    lazy val toBytes = newFeature(
//      (x: Byte) => x.toBytes,
//      (x: Byte) => x.toBytes,
//      "{ (x: Byte) => x.toBytes }")
//    val cases = Seq(
//      (0.toByte, Success(Coll(0.toByte))),
//      (1.toByte, Success(Coll(1.toByte)))
//    )
//    testCases(cases, toBytes)
//  }
}
