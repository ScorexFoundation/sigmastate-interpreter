package sigmastate.serialization

import java.math.BigInteger

import sigmastate.Values.GroupElementConstant
import com.google.common.primitives.Shorts
import scapi.sigma.GroupAgnosticEcElement
import sigmastate.interpreter.GroupSettings
import sigmastate.serialization.OpCodes._


//todo: for now, no any compression is used, fix it when concrete group is known
object GroupElementSerializer extends ValueSerializer[GroupElementConstant] {

  type ElemType = GroupSettings.EcPointType

  private val curve = GroupSettings.dlogGroup

  override val opCode: OpCode = GroupElementConstantCode

  override def serializeBody(gec: GroupElementConstant): Array[Byte] = {
    val point = gec.value.normalize()

    val xCoord = point.getAffineXCoord.toBigInteger.toByteArray
    val yCoord = point.getAffineYCoord.toBigInteger.toByteArray

    val xSize = xCoord.size.toShort
    val ySize = yCoord.size.toShort

    Shorts.toByteArray(xSize) ++
      Shorts.toByteArray(ySize) ++
      xCoord ++
      yCoord
  }

  override def parseBody(bytes: Array[Byte], pos: Int): (GroupElementConstant, Int) = {
    val xSize = Shorts.fromByteArray(bytes.slice(pos, pos + 2))
    val ySize = Shorts.fromByteArray(bytes.slice(pos + 2, pos + 4))

    val xCoord = new BigInteger(bytes.slice(pos + 4, pos + 4 + xSize))
    val yCoord = new BigInteger(bytes.slice(pos + 4 + xSize, pos + 4 + xSize + ySize))

    (GroupElementConstant(curve.reconstructElement(bCheckMembership = true,
      GroupAgnosticEcElement(xCoord, yCoord)).get), 4 + xSize + ySize)
  }
}
