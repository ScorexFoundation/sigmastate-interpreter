package special.collections

import scalan.RType
import special.collection.{Coll, CollBuilder}

import scala.reflect.{ClassTag, classTag}

class Examples(builder: CollBuilder) {
  import builder._
  import Examples._
  import special.collection.ExtensionMethods._
  implicit val longMonoid = Monoids.longPlusMonoid

  def checkTokenBalance(ctx: Context): Boolean = {
    val input = ctx.inputs.flatMap(_.tokens).sumByKey
    val output = ctx.outputs.flatMap(_.tokens).sumByKey
    val flagged = outerJoin(input, output)(
      onlyIn => false,
      onlyOut => false,
      { case (tokenId, (inV, outV)) => inV == outV })
    flagged.forall { case (tokenId, ok) => ok }
  }

  def tokenTotal1(ctx: Context, tokenId: TokenId): Long = {
    val tokenValues = ctx.inputs.flatMap(box => box.tokens.filter(t => t._1 == tokenId).map(t => t._2))
    tokenValues.sum(longMonoid)
  }
  def tokenTotal2(ctx: Context, tokenId: TokenId): Long = {
    val tokenRecords = ctx.inputs.flatMap(box => box.tokens)
    val selectedTokens = tokenRecords.filter(t => t._1 == tokenId)
    selectedTokens.map(_._2).sum(longMonoid)
  }
  def tokenTotal3(ctx: Context, tokenId: TokenId): Long = {
    val tokenValues = ctx.inputs.map { box =>
      val optToken = box.tokens.find(t => t._1 == tokenId)
      optToken.map(t => t._2).getOrElse(0L)
    }
    tokenValues.sum(longMonoid)
  }
}

object Examples {
  type IdType = Coll[Byte]
  type TokenId = IdType
  type TokenData = (TokenId, Long)
  case class Box(id: IdType, tokens: Coll[TokenData]) {
    override def toString: String = {
      val idVal = id.toArray(0)
      val tokenStr = tokens.map(t => s"${t._1.toArray(0)}->${t._2}").toArray.mkString("[", ";", "]")
      s"Box{id=$idVal; tokens=$tokenStr}"
    }
  }
  implicit val boxType = RType.fromClassTag(classTag[Box])
  case class Context(inputs: Coll[Box], outputs: Coll[Box], selfBoxIndex: Int) {
    override def toString: String = {
      s"""Ctx {
        |  inputs=[
        |    ${inputs.toArray.mkString(";\n    ")}
        |  ]
        |  outputs=[
        |    ${outputs.toArray.mkString(";\n    ")}
        |  ]
        |  self=$selfBoxIndex
        |}
       """.stripMargin
    }
  }
}
