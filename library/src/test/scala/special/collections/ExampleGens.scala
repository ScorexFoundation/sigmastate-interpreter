package special.collections

import special.collection.Coll
import scalan.util.CollectionUtil._
import scalan.RType._

trait ExampleGens extends CollGens {
  import Examples._
  import org.scalacheck.Gen._
  import org.scalacheck.Arbitrary._

  val genId = containerOfN[Coll, Byte](1, choose[Byte](1, Byte.MaxValue))
  val availableTokenIds = Range(0,10).map(_ => genId.sample.get).distinct

  val genTokenId = oneOf(availableTokenIds)
  val genTokenValue = choose[Long](0, 100)
  val genTokenData = for {
      id <- genTokenId
      value <- genTokenValue
    } yield (id, value)
    
  val genBox = for {
      id <- genId
      tokens <- containerOfN[Coll, TokenData](5, genTokenData).map(_.distinctByKey)
    } yield Box(id, tokens)

  val genContext = for {
      inputs <- containerOfN[Coll, Box](5, genBox)
      outputs <- containerOfN[Coll, Box](5, genBox)
      iSelf <- choose(0, inputs.length)
    }
    yield Context(inputs, outputs, iSelf)
}
