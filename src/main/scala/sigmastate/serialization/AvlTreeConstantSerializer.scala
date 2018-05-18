package sigmastate.serialization

import com.google.common.primitives.Ints
import scorex.crypto.authds.ADDigest
import sigmastate.AvlTreeData
import sigmastate.Values.AvlTreeConstant
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.Serializer.{Position, Consumed}

object AvlTreeConstantSerializer extends ValueSerializer[AvlTreeConstant] {
  override val opCode: OpCode = OpCodes.AvlTreeConstantCode

  val IntLength = Ints.BYTES

  override def parseBody(bytes: Array[Byte], pos: Position): (AvlTreeConstant, Consumed) = {
    val digestLength = Ints.fromByteArray(bytes.slice(pos, pos + IntLength))

    val digestStart = pos + IntLength

    val digestBytes = bytes.slice(digestStart, digestStart + digestLength)
    val keyLengthStart = digestStart + digestLength
    val keyLength = Ints.fromByteArray(bytes.slice(keyLengthStart, keyLengthStart + IntLength))

    val vlOptStart = keyLengthStart + IntLength
    val vlOpt = read(bytes.slice(vlOptStart, bytes.length))
    val mnoOptStart = vlOptStart + vlOpt.fold(1) { _ => IntLength + 1 }
    val mnoOpt = read(bytes.slice(mnoOptStart, bytes.length))
    val mdOptStart = mnoOptStart + mnoOpt.fold(1) { _ => IntLength + 1 }
    val mdOpt = read(bytes.slice(mdOptStart, bytes.length))

    val consumed = mdOptStart + mdOpt.fold(1) { _ => IntLength + 1 }
    val data = AvlTreeData(ADDigest @@ digestBytes, keyLength, vlOpt, mnoOpt, mdOpt)
    AvlTreeConstant(data) -> consumed
  }

  override def serializeBody(obj: AvlTreeConstant): Array[Byte] = {
    val data = obj.value
    val digestLength = data.startingDigest.length
    val digestBytes = Ints.toByteArray(digestLength) ++ data.startingDigest
    val keyLengthBytes = Ints.toByteArray(data.keyLength)

    val valueLengthBytes = optionIntToBytes(data.valueLengthOpt)
    val maxNumOpBytes = optionIntToBytes(data.maxNumOperations)
    val maxDeletesBytes = optionIntToBytes(data.maxDeletes)

    digestBytes ++ keyLengthBytes ++ valueLengthBytes ++ maxNumOpBytes ++ maxDeletesBytes
  }


  private def optionIntToBytes(opt: Option[Int]): Array[Byte] =
    opt.fold(Array(0: Byte)) { v => Array(1: Byte) ++ Ints.toByteArray(v) }

  private def read(bytes: Array[Byte]): Option[Int] = bytes.headOption.flatMap {
    case x: Byte if x == 0 => None
    case _ => Some(Ints.fromByteArray(bytes.drop(1)))
  }

}
