package sigmastate.serialization.generators

import org.ergoplatform._
import org.ergoplatform.validation.ValidationSpecification
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import scorex.util.encode.{Base64, Base58}
import sigmastate._
import sigmastate.Values.{TrueLeaf, Value, IntConstant, _}
import sigmastate.lang.TransformingSigmaBuilder
import sigmastate.utxo._

trait TransformerGenerators extends ValidationSpecification {
  self: ObjectGenerators with ConcreteCollectionGenerators =>

  import TransformingSigmaBuilder._

}
