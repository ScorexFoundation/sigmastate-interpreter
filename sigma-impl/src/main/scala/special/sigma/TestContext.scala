package special.sigma

import scala.reflect.ClassTag
import scalan.{NeverInline, RType}
import special.collection.Coll

class TestValue[A](val value: A)(implicit val tA: RType[A]) extends AnyValue {
  def dataSize = SigmaPredef.dataSize(value)
  override def toString = s"Value($value)"
}

class TestContext(
    private[sigma] val _inputs: Array[Box],
    private[sigma] val _outputs: Array[Box],
    private[sigma] val _height: Int,
    private[sigma] val _selfBox: Box,
    private[sigma] val _lastBlockUtxoRootHash: AvlTree,
    private[sigma] val _minerPubKey: Array[Byte],
    private[sigma] val _vars: Array[AnyValue]
) extends Context {
  def builder: SigmaDslBuilder = new TestSigmaDslBuilder
  def HEIGHT = _height
  def SELF   = _selfBox
  def dataInputs: Coll[Box] = sys.error(s"dataInputs in not defined for TestContext")
  def INPUTS = builder.Colls.fromArray(_inputs)
  def OUTPUTS = builder.Colls.fromArray(_outputs)
  def LastBlockUtxoRootHash = _lastBlockUtxoRootHash

  def minerPubKey = builder.Colls.fromArray(_minerPubKey)

  def getVar[T](id: Byte)(implicit cT: RType[T]): Option[T] = {
    implicit val tag: ClassTag[T] = cT.classTag
    if (id < 0 || id >= _vars.length) return None
    val value = _vars(id)
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

  private[sigma] def cost = (dataSize / builder.CostModel.AccessKiloByteOfData.toLong).toInt

  private[sigma] def dataSize = {
    val inputsSize = INPUTS.map(_.dataSize).sum(builder.Monoids.longPlusMonoid)
    val outputsSize = OUTPUTS.map(_.dataSize).sum(builder.Monoids.longPlusMonoid)
    8L + (if (SELF == null) 0 else SELF.dataSize) + inputsSize + outputsSize + LastBlockUtxoRootHash.dataSize
  }

  def findSelfBoxIndex: Int = {
    var i = 0
    while (i < _inputs.length) {
      if (_inputs(i) eq _selfBox) return i
      i += 1
    }
    -1
  }

  override val selfBoxIndex: Int = findSelfBoxIndex

  override def headers: Coll[Header] = sys.error(s"headers in not defined for TestContext")
  override def preHeader: PreHeader = sys.error(s"preHeader in not defined for TestContext")
}
