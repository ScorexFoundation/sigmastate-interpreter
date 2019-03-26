package special.sigma

import org.scalacheck.{Arbitrary, Gen}
import sigmastate.eval.CBigInt
import sigmastate.serialization.generators.ValueGenerators

trait SigmaTypeGens extends ValueGenerators {
  import Gen._; import Arbitrary._
  import sigma.types._
  val genBoolean = Arbitrary.arbBool.arbitrary.map(CBoolean(_): Boolean)
  implicit val arbBoolean = Arbitrary(genBoolean)

  val genByte = Arbitrary.arbByte.arbitrary.map(CByte(_): Byte)
  implicit val arbByte = Arbitrary(genByte)

  val genInt = Arbitrary.arbInt.arbitrary.map(CInt(_): Int)
  implicit val arbInt = Arbitrary(genInt)
}

