package sigmastate.utxo

import sigmastate.lang.SigmaParser
import sigmastate.lang.Terms.OperationId

case class CostTable(operCosts: Map[OperationId, Double]) extends (OperationId => Int) {
  override def apply(operId: OperationId) = {
    operCosts.get(operId) match {
      case Some(cost) => (cost * 1000000).toInt
      case None => sys.error(s"Cannot find cost in CostTable for $operId")
    }
  }
}

object CostTable {
  type ExpressionCost = Int
  val DefaultCosts = CostTable.fromSeq(Seq(
    ("Const", "() => Unit",    0.000001),
    ("Const", "() => Boolean", 0.000001),
    ("Const", "() => Byte",    0.000001),
    ("Const", "() => Short",   0.000001),
    ("Const", "() => Int",     0.000001),
    ("Const", "() => Long",    0.000001),
    ("Const", "() => BigInt",  0.000001),
    ("Const", "() => String",  0.000001),
    ("Const", "() => GroupElement", 0.000001),
    ("Const", "() => SigmaProp", 0.000001),
    ("Const", "() => Array[IV]", 0.000001),
    ("SigmaPropIsValid", "SigmaProp => Boolean", 0.000001),
    ("SigmaPropBytes", "SigmaProp => Array[Byte]", 0.000001),
    ("BinAnd", "(Boolean, Boolean) => Boolean", 0.000001),
    ("BinOr", "(Boolean, Boolean) => Boolean", 0.000001),
    ("+", "(BigInt, BigInt) => BigInt", 0.0001),
    ("+_per_item", "(BigInt, BigInt) => BigInt", 0.000001)
  ))

  def fromSeq(items: Seq[(String, String, Double)]): CostTable = {
    val parsed = for ((name, ts, cost) <- items) yield {
      val ty = SigmaParser.parseType(ts).asFunc
      (OperationId(name, ty), cost)
    }
    CostTable(parsed.toMap)
  }

  //Maximum cost of a script
  val ScriptLimit = 1000000

  //Maximum number of expressions in initial(non-reduced script)
  val MaxExpressions = 300

  object Cost {
    val ConstantNode = 1

    val HeightAccess = 1
    val InputsAccess = 1
    val OutputsAccess = 1
    val SelfAccess = 1
    val VariableAccess = 1

    val ExtractAmount = 10
    val ExtractScriptBytes = 10
    val ExtractRegister = 10

    /** The cost for CustomByteArray declaration. Additional cost to be calculated when data is known
     (and CustomByteArray being converted to ByteArrayLeaf) */
    val ByteArrayDeclaration = 1

    val ByteArrayPerKilobyte = 200

    val BoxPerKilobyte = 50

    val TripleDeclaration = 3

    val QuadrupleDeclaration = 4

    val BooleanConstantDeclaration = 1
    val ByteConstantDeclaration = 1
    val ShortConstantDeclaration = 1
    val IntConstantDeclaration = 1
    val LongConstantDeclaration = 1
    val BigIntConstantDeclaration = 1
    val StringConstantDeclaration = 1
    val GroupElementConstantDeclaration = 10
    val SigmaPropConstantDeclaration = 10
    val BoxConstantDeclaration = 10
    val AvlTreeConstantDeclaration = 50

    val AndDeclaration = 10
    val AndPerChild = 1

    val OrDeclaration = 1
    val OrPerChild = 1

    val BinOrDeclaration = 1
    val BinAndDeclaration = 1
    val IfDeclaration = 1

    /**PropLeaf declaration cost, wrapped script cost to be added as well.*/
    val PropLeafDeclaration = 500

    /** Cost of Blake256 declaration */
    val Blake256bDeclaration = 20

    val DlogDeclaration = 10000

    val TxHasOutputDeclaration = 100
    val TxOutputDeclaration = 100

    val SizeOfDeclaration = 30
    val ByIndexDeclaration = 50
    val SelectFieldDeclaration = 50
    val SigmaPropIsValidDeclaration = 50
    val SigmaPropBytesDeclaration = 50

    val MapDeclaration = 100
    val WhereDeclaration = 200
    val ExistsDeclaration = 200
    val ForAllDeclaration = 200
    val FoldDeclaration = 200

    val ConcreteCollectionDeclaration = 20
    val TupleDeclaration = 20
    val LambdaDeclaration = 1


    val Exponentiate = 5000
    val MultiplyGroup = 50
  }
}
