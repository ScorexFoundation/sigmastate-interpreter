package special.sigma

case class CostTable(operCosts: Map[String, Double]) extends (String => Int) {
  override def apply(operId: String): Int = {
    operCosts.get(operId) match {
      case Some(cost) => (cost * 1000000).toInt
      case None => sys.error(s"Cannot find cost in CostTable for $operId")
    }
  }
}

object CostTable {
  type ExpressionCost = Int
  val DefaultCosts = CostTable.fromSeq(Seq(
    ("Const: () => Unit",    0.000001),
    ("Const: () => Boolean", 0.000001),
    ("Const: () => Byte",    0.000001),
    ("Const: () => Short",   0.000001),
    ("Const: () => Int",     0.000001),
    ("Const: () => Long",    0.000001),
    ("Const: () => BigInt",  0.000001),
    ("Const: () => String",  0.000001),
    ("Const: () => GroupElement", 0.000001),
    ("Const: () => SigmaProp", 0.000001),
    ("Const: () => Array[IV]", 0.000001),
    ("Self$: Context => Box", 0.000001),
    ("SelectField", 0.000001),
    ("AccessKiloByteOfData", 0.000001),
    ("AccessBox: Context => Box", 0.000001),

    ("GetVar: (Context, Byte) => Option[T]", 0.000001),
    ("DeserializeVar: (Context, Byte) => Option[T]", 0.000001),

    ("GetRegister: (Box, Byte) => Option[T]", 0.000001),
    ("DeserializeRegister: (Box, Byte) => Option[T]", 0.000001),

    ("ExtractRegisterAs: (Box,Byte) => Array[BigInt]", 0.000001),
    ("SigmaPropIsValid: SigmaProp => Boolean", 0.000001),
    ("SigmaPropBytes: SigmaProp => Array[Byte]", 0.000001),
    ("BinAnd: (Boolean, Boolean) => Boolean", 0.000001),
    ("BinOr: (Boolean, Boolean) => Boolean", 0.000001),
    ("BinXor: (Boolean, Boolean) => Boolean", 0.000001),
    ("+: (BigInt, BigInt) => BigInt", 0.0001),
    ("+_per_item: (BigInt, BigInt) => BigInt", 0.000001)
  ))

  def fromSeq(items: Seq[(String, Double)]): CostTable = {
    CostTable(items.toMap)
  }
}


