package sigmastate.serialization.generators

import org.scalacheck.{Arbitrary, Gen}
import sigmastate._
import sigmastate.Values.{ConcreteCollection, EvaluatedValue, Value}

trait ConcreteCollectionGenerators { self: ValueGenerators =>
  val minCollLength = 1
  val maxCollLength = 10

  def concreteCollectionGen[T <: SType](implicit constGen: Gen[Value[T]]): Gen[ConcreteCollection[T]] =
    for {
      size <- Gen.chooseNum(minCollLength, maxCollLength)
      c <- constGen
      listOfConsts <- Gen.listOfN(size, constGen)
    } yield ConcreteCollection(listOfConsts.toIndexedSeq)(c.tpe)

  val intConstCollectionGen: Gen[ConcreteCollection[SInt.type]] = for {
    size <- Gen.chooseNum(minCollLength, maxCollLength)
    listOfConstInts <- Gen.listOfN(size, intConstGen)
  } yield ConcreteCollection(listOfConstInts.toIndexedSeq)

  implicit val arbCCOfIntConstant: Arbitrary[ConcreteCollection[SInt.type]] = Arbitrary(intConstCollectionGen)
  implicit val arbCCOfBoolConstant: Arbitrary[ConcreteCollection[SBoolean.type]] = Arbitrary(concreteCollectionGen[SBoolean.type](booleanConstGen))
  implicit val arbCCOfSigmaPropConstant: Arbitrary[ConcreteCollection[SSigmaProp.type]] = Arbitrary(concreteCollectionGen[SSigmaProp.type](sigmaPropValueGen))
}
