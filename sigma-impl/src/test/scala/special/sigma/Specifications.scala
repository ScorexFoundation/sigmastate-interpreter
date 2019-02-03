package special.sigma

import scalan.RType
import special.collection.Coll
trait ContractSyntax { contract: SigmaContract =>
  val syntax = new ExtensionMethods(builder)
  def Coll[T](items: T*)(implicit cT: RType[T]) = builder.Colls.fromItems(items:_*)
}

class MySpec(pkA: SigmaProp, deadline: Int, token1: Coll[Byte]) extends DefaultContract with ContractSyntax {
  import syntax._

  def altBuyerProp(ctx: Context): SigmaProp = {
    import ctx._
    (HEIGHT > deadline && pkA) || {
      val tokenData = OUTPUTS(0).getReg[Coll[(Coll[Byte], Long)]](2).get(0)
      allOf(Coll(
        tokenData._1 == token1,
        tokenData._2 >= 60L,
        OUTPUTS(0).propositionBytes == pkA.propBytes,
        OUTPUTS(0).getReg[Coll[Byte]](4).get == SELF.id
      ))
    }
  }
}
