package sigmastate.serialization.generators

import org.scalacheck.{Arbitrary, Gen}
import sigmastate.{SInt, SType}
import sigmastate.Values.{ConcreteCollection, EvaluatedValue}

trait ConcreteCollectionGenerators { self: ValueGenerators =>
  val minCollLength = 1
  val maxCollLength = 10

  def concreteCollectionGen[T <: SType](implicit constGen: Gen[EvaluatedValue[T]]): Gen[ConcreteCollection[T]] =
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
}
