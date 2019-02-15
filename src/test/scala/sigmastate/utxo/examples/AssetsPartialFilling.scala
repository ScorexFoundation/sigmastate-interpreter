package sigmastate.utxo.examples

import org.ergoplatform.dsl.ContractSyntax.Token
import special.sigma.Context
import org.ergoplatform.ErgoBox.R4
import special.collection.Coll
import org.ergoplatform.dsl.{SigmaContractSyntax, ContractSpec, StdContracts}

abstract class AssetsPartialFilling[Spec <: ContractSpec]
    (val deadline: Int, val token1: Coll[Byte])
    (implicit val spec: Spec)
    extends SigmaContractSyntax with StdContracts
{
  /** The party, who wants to buy some amount of token with id == `tokenId`. */
  val tokenBuyer: spec.ProvingParty
  /** The party, who wants to sell some amount of token with id == `tokenId`. */
  val tokenSeller: spec.ProvingParty
  val verifier: spec.VerifyingParty
  def pkA = tokenBuyer.pubKey
  def pkB = tokenSeller.pubKey
  import syntax._
  lazy val env = Env("pkA" -> pkA, "pkB" -> pkB, "deadline" -> deadline, "token1" -> token1)

  lazy val buyerProp = proposition("buyer", { ctx: Context =>
    import ctx._
    (HEIGHT > deadline && pkA) || {

      val outIdx = getVar[Short](127).get
      val out = OUTPUTS(outIdx)
      val tokenData = out.R2[Coll[(Coll[Byte], Long)]].get(0)
      val tokenId = tokenData._1
      val tokenValue = tokenData._2
      val outValue = out.value
      val price = 500

      allOf(Coll(
        tokenId == token1,
        tokenValue >= 1,
        (SELF.value - outValue) <= tokenValue * price,
        out.propositionBytes == pkA.propBytes,
        out.R4[Coll[Byte]].get == SELF.id
      ))
    }
  },
  env,
  """(HEIGHT > deadline && pkA) || {
   |
   |  val outIdx = getVar[Short](127).get
   |  val out = OUTPUTS(outIdx)
   |  val tokenData = out.R2[Coll[(Coll[Byte], Long)]].get(0)
   |  val tokenId = tokenData._1
   |  val tokenValue = tokenData._2
   |  val outValue = out.value
   |  val price = 500
   |
   |  allOf(Coll(
   |      tokenId == token1,
   |      tokenValue >= 1,
   |      (SELF.value - outValue) <= tokenValue * price,
   |      out.propositionBytes == pkA.propBytes,
   |      out.R4[Coll[Byte]].get == SELF.id
   |  ))
   |}
  """.stripMargin)

  lazy val sellerProp = proposition("seller", {ctx: Context =>
    import ctx._
    (HEIGHT > deadline && pkB) || {
      val outIdx = getVar[Short](127).get
      val out = OUTPUTS(outIdx)

      val tokenData = out.R2[Coll[(Coll[Byte], Long)]].get(0)
      val tokenId = tokenData._1
      val tokenValue = tokenData._2

      val selfTokenData = SELF.R2[Coll[(Coll[Byte], Long)]].get(0)
      val selfTokenId = selfTokenData._1
      val selfTokenValue = selfTokenData._2

      val selfValue = SELF.value
      val outValue = out.value

      val sold = selfTokenValue - tokenValue

      val price = 495
      val outR4 = out.R4[Coll[Byte]].get

      allOf(Coll(
        sold >= 1,
        (outValue - selfValue) >= sold*price,
        outR4 == SELF.id,
        out.propositionBytes == pkB.propBytes
      ))
    }
  },
  env,
  """ (HEIGHT > deadline && pkB) || {
   |   val outIdx = getVar[Short](127).get
   |   val out = OUTPUTS(outIdx)
   |
   |   val tokenData = out.R2[Coll[(Coll[Byte], Long)]].get(0)
   |   val tokenId = tokenData._1
   |   val tokenValue = tokenData._2
   |
   |   val selfTokenData = SELF.R2[Coll[(Coll[Byte], Long)]].get(0)
   |   val selfTokenId = selfTokenData._1
   |   val selfTokenValue = selfTokenData._2
   |
   |   val selfValue = SELF.value
   |   val outValue = out.value
   |
   |   val sold = selfTokenValue - tokenValue
   |
   |   val price = 495
   |
   |   allOf(Coll(
   |        sold >= 1,
   |        (outValue - selfValue) >= sold*price,
   |        out.R4[Coll[Byte]].get == SELF.id,
   |        out.propositionBytes == pkB.propBytes
   |   ))
   | }
  """.stripMargin)

  lazy val buyerSignature  = proposition("buyerSignature", _ => pkA, env, "pkA")
  lazy val sellerSignature = proposition("sellerSignature", _ => pkB, env, "pkB")

  import spec._

  /** This methods starts exchange protocol using two boxes.
    * It requires that the boxes are protected by `buyerSignature` and `sellerSignature` correspondingly.
    * It creates a transaction in the target block with two holder boxes and two change boxes.
    * @return a pair of holder boxes
    */
  def startExchange(targetBlock: Block, buyerErgBox: OutBox, sellerTokenBox: OutBox, ergAmt: Long, tokenAmt: Token): (OutBox, OutBox) = {
    require(buyerErgBox.propSpec == buyerSignature && sellerTokenBox.propSpec == sellerSignature)

    val tx = targetBlock.newTransaction().spending(buyerErgBox, sellerTokenBox)
    val (buyerHolder, _) = transferErgWithChange(tx, buyerErgBox, buyerProp, ergAmt)
    val (sellerHolder, _) = transferTokenWithChange(tx, sellerTokenBox, sellerProp, tokenAmt)
    (buyerHolder, sellerHolder)
  }

}
