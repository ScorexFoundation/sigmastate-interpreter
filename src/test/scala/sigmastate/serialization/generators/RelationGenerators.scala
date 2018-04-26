package sigmastate.serialization.generators

import org.scalacheck.{Arbitrary, Gen}
import sigmastate.IsMember

trait RelationGenerators { this: ValueGeneratots with ConcreteCollectionGenerators =>

  implicit val arbIsMember: Arbitrary[IsMember] = Arbitrary(isMemberGen)

  val isMemberGen: Gen[IsMember] = for {
    t <- arbAvlTree.arbitrary
    b1 <- arbByteArrayConstant.arbitrary
    b2 <- arbByteArrayConstant.arbitrary
  } yield IsMember(t, b1, b2)
}
