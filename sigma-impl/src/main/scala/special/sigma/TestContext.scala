package special.sigma

import scala.reflect.ClassTag
import scalan.{NeverInline, RType}
import special.collection.Coll

class TestValue[A](val value: A)(implicit val tA: RType[A]) extends AnyValue {
  def dataSize = SigmaPredef.dataSize(value)
  override def toString = s"Value($value)"
}

class TestContext(
    val inputs: Array[Box],
    val outputs: Array[Box],
    val height: Int,
    val selfBox: Box,
    val lastBlockUtxoRootHash: AvlTree,
    val minerPubKey: Array[Byte],
    val vars: Array[AnyValue]
) extends Context {
  def builder = new TestSigmaDslBuilder
  def HEIGHT = height
  def SELF   = selfBox
  def INPUTS = builder.Colls.fromArray(inputs)
  def OUTPUTS = builder.Colls.fromArray(outputs)
  def LastBlockUtxoRootHash = lastBlockUtxoRootHash
  def MinerPubKey = builder.Colls.fromArray(minerPubKey)
  def getVar[T](id: Byte)(implicit cT: RType[T]): Option[T] = {
    implicit val tag: ClassTag[T] = cT.classTag
    if (id < 0 || id >= vars.length) return None
    val value = vars(id)
    if (value != null ) {
      // once the value is not null it should be of the right type
      value match {
        case value: TestValue[_] if value.value != null && value.tA == cT =>
          Some(value.value.asInstanceOf[T])
        case _ =>
          throw new InvalidType(s"Cannot getVar[${cT.name}]($id): invalid type of value $value at id=$id")
      }
    } else None
  }

  def getConstant[T](id: Byte)(implicit cT: RType[T]): T =
    sys.error(s"Method getConstant is not defined in TestContext. Should be overriden in real context.")

  def cost = (dataSize / builder.CostModel.AccessKiloByteOfData.toLong).toInt

  def dataSize = {
    val inputsSize = INPUTS.map(_.dataSize).sum(builder.Monoids.longPlusMonoid)
    val outputsSize = OUTPUTS.map(_.dataSize).sum(builder.Monoids.longPlusMonoid)
    8L + (if (SELF == null) 0 else SELF.dataSize) + inputsSize + outputsSize + LastBlockUtxoRootHash.dataSize
  }
  override def selfBoxIndex: Int = ???
  override def headers: Coll[Header] = ???
  override def preheader: Preheader = ???
}
