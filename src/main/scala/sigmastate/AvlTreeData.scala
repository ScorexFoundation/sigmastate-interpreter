package sigmastate

import scorex.crypto.authds.ADDigest

class AvlTreeData(val startingDigest: ADDigest,
                  val keyLength: Int,
                  val valueLengthOpt: Option[Int] = None,
                  val maxNumOperations: Option[Int] = None,
                  val maxDeletes: Option[Int] = None)

object AvlTreeData {
  val dummy = new AvlTreeData(ADDigest @@ Array.fill(32)(0:Byte), keyLength = 32)
}
