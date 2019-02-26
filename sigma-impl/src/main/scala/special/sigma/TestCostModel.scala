package special.sigma

import scala.reflect.ClassTag
import scalan.Internal

@Internal
class TestCostModel extends CostModel {
  def AccessBox: Int = CostTable.DefaultCosts("AccessBox: Context => Box")
  def AccessAvlTree: Int = CostTable.DefaultCosts("AccessAvlTree: Context => AvlTree")

  def GetVar: Int = CostTable.DefaultCosts("GetVar: (Context, Byte) => Option[T]")
  def DeserializeVar: Int = CostTable.DefaultCosts("DeserializeVar: (Context, Byte) => Option[T]")

  def GetRegister: Int = CostTable.DefaultCosts("GetRegister: (Box, Byte) => Option[T]")
  def DeserializeRegister: Int  = CostTable.DefaultCosts("DeserializeRegister: (Box, Byte) => Option[T]")

  def SelectField: Int      = CostTable.DefaultCosts("SelectField")
  def CollectionConst: Int  = CostTable.DefaultCosts("Const: () => Array[IV]")
  def AccessKiloByteOfData: Int  = CostTable.DefaultCosts("AccessKiloByteOfData")
  def dataSize[T](x: T)(implicit cT: ClassTag[T]): Long = SigmaPredef.dataSize(x)
}
