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
  implicit val proofTypeGen = Gen.const(SProof)
  implicit val boxTypeGen = Gen.const(SBox)
  implicit val avlTreeTypeGen = Gen.const(SAvlTree)

  implicit val primTypeGen: Gen[SPrimType] =
    Gen.oneOf[SPrimType](SBoolean, SByte, SShort, SInt, SLong, SBigInt, SGroupElement, SProof, SUnit)
  implicit val arbPrimType = Arbitrary(primTypeGen)

  implicit val predefTypeGen: Gen[SPredefType] =
    Gen.oneOf[SPredefType](SBoolean, SByte, SShort, SInt, SLong, SBigInt, SGroupElement, SProof, SUnit, SBox, SAvlTree)
  implicit val arbPredefType = Arbitrary(predefTypeGen)

  implicit def getToArbitrary[T: Gen]: Arbitrary[T] = Arbitrary(implicitly[Gen[T]])

  def sTupleGen(min: Int, max: Int): Gen[STuple] = for {
    length <- Gen.chooseNum(min, max)
    values <- Gen.listOfN[SType](length, Gen.oneOf(
      byteTypeGen,
      shortTypeGen,
      intTypeGen,
      longTypeGen,
      bigIntTypeGen,
    ))
  } yield STuple(values.toIndexedSeq)
}
