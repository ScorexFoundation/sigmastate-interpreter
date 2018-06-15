package sigmastate.serialization.generators

import org.scalacheck.{Arbitrary, Gen}
import sigmastate._

trait TypeGenerators {
  implicit val booleanTypeGen = Gen.const(SBoolean)
  implicit val byteTypeGen = Gen.const(SByte)
  implicit val shortTypeGen = Gen.const(SShort)
  implicit val intTypeGen = Gen.const(SInt)
  implicit val longTypeGen = Gen.const(SLong)
  implicit val bigIntTypeGen = Gen.const(SBigInt)
  implicit val groupElementTypeGen = Gen.const(SGroupElement)
  implicit val boxTypeGen = Gen.const(SBox)
  implicit val avlTreeTypeGen = Gen.const(SAvlTree)

  implicit val primTypeGen: Gen[SPredefType] =
    Gen.oneOf[SPredefType](SBoolean, SByte, SShort, SInt, SLong, SBigInt, SGroupElement, SUnit, SBox, SAvlTree)
  implicit val arbPrimType = Arbitrary(primTypeGen)

  implicit def getToArbitrary[T: Gen]: Arbitrary[T] = Arbitrary(implicitly[Gen[T]])
}
