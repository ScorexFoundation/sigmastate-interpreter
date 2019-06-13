package sigmastate.serialization.generators

import org.scalacheck.{Arbitrary, Gen}
import sigmastate.Values.{FalseLeaf, TrueLeaf}
import sigmastate.{If, SInt, TreeLookup}

trait RelationGenerators {
  this: ObjectGenerators with ConcreteCollectionGenerators =>

  implicit val arbTreeLookup: Arbitrary[TreeLookup] = Arbitrary(treeLookupGen)
  implicit val arbIf: Arbitrary[If[SInt.type]] = Arbitrary(ifGen)

  val treeLookupGen: Gen[TreeLookup] = for {
    t <- arbTaggedAvlTree.arbitrary
    b1 <- arbByteArrayConstant.arbitrary
    b2 <- arbByteArrayConstant.arbitrary
  } yield TreeLookup(t, b1, b2)

  val ifGen: Gen[If[SInt.type]] = for {
    c <- Gen.oneOf(TrueLeaf, FalseLeaf)
    tb <- arbIntConstants.arbitrary
    fb <- arbIntConstants.arbitrary
  } yield If(c, tb, fb)


}
