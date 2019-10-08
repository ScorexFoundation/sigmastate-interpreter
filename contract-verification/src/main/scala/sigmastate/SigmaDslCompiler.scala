package sigmastate

import sigmastate.verification.SigmaDsl.api.sigma.{Context, SigmaProp}
import stainless.annotation.ignore

@ignore
object SigmaDslCompiler {

  def compile(contract: Context => Boolean): SigmaProp = ???

  def serializedErgoTree(contract: Context => Boolean): Array[Byte] = ???

  def PK(p2PKAddress: String): SigmaProp =
    ErgoAddressEncode(networkPrefix).fromString(p2PKAddress).get match {
      case a: P2PKAddress => SigmaPropConstant(a.pubkey)
      case a@_ => sys.error(s"unsupported address $a")
    }

}
