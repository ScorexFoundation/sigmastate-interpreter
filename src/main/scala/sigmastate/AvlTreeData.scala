package sigmastate

import java.util.{Objects, Arrays}
import scorex.crypto.authds.ADDigest

case class AvlTreeData( startingDigest: ADDigest,
                        keyLength: Int,
                        valueLengthOpt: Option[Int] = None,
                        maxNumOperations: Option[Int] = None,
                        maxDeletes: Option[Int] = None ) {
  override def equals(arg: Any) = arg match {
    case x: AvlTreeData =>
      Arrays.equals(startingDigest, x.startingDigest) &&
      keyLength == x.keyLength &&
      valueLengthOpt == x.valueLengthOpt &&
      maxNumOperations == x.maxNumOperations &&
      maxDeletes == x.maxDeletes
    case _ => false
  }

  override def hashCode() =
    (Arrays.hashCode(startingDigest) * 31 +
        keyLength.hashCode()) * 31 + Objects.hash(valueLengthOpt, maxNumOperations, maxDeletes)
}

object AvlTreeData {
  val dummy = new AvlTreeData(ADDigest @@ Array.fill(32)(0:Byte), keyLength = 32)
}
