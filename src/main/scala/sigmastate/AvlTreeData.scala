package sigmastate

import scorex.crypto.authds.ADDigest

case class AvlTreeData( startingDigest: ADDigest,
                        keyLength: Int,
                        valueLengthOpt: Option[Int] = None,
                        maxNumOperations: Option[Int] = None,
                        maxDeletes: Option[Int] = None )

object AvlTreeData {
  val dummy = new AvlTreeData(ADDigest @@ Array.fill(32)(0:Byte), keyLength = 32)
}
