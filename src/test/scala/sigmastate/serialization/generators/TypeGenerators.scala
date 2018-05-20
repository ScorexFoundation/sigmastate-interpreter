package sigmastate.serialization.generators

import org.scalacheck.{Arbitrary, Gen}
import sigmastate._

trait TypeGenerators {
  implicit val byteTypeGen = Gen.const(SByte)
  implicit val booleanTypeGen = Gen.const(SBoolean)
  implicit val intTypeGen = Gen.const(SInt)
  implicit val bigIntTypeGen = Gen.const(SBigInt)
  implicit val groupElementTypeGen = Gen.const(SGroupElement)
  implicit val boxTypeGen = Gen.const(SBox)
  implicit val avlTreeTypeGen = Gen.const(SAvlTree)
  implicit def getToArbitrary[T: Gen]: Arbitrary[T] = Arbitrary(implicitly[Gen[T]])
}
