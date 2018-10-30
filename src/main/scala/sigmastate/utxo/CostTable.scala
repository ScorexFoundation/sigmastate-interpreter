package sigmastate.utxo

import sigmastate.{Downcast, Upcast}
import sigmastate.lang.SigmaParser
import sigmastate.lang.Terms.OperationId

case class CostTable(operCosts: Map[OperationId, Double]) extends (OperationId => Int) {
  import CostTable._
  override def apply(operId: OperationId) = {
    val cleannedOperId = operId.copy(opType = operId.opType.copy(tpeParams = Nil))
    operCosts.get(cleannedOperId) match {
      case Some(cost) => costToInt(cost)
      case None => //costToInt(MinimalCost)
        sys.error(s"Cannot find cost in CostTable for $operId")
    }
  }
}

object CostTable {
  type ExpressionCost = Int
  def costToInt(cost: Double): Int = (cost * 1000000).toInt
  val MinimalCost = 0.000001
  val DefaultCosts = CostTable.fromSeq(Seq(
    ("Const", "() => Unit",    MinimalCost),
    ("Const", "() => Boolean", MinimalCost),
    ("Const", "() => Byte",    MinimalCost),
    ("Const", "() => Short",   MinimalCost),
    ("Const", "() => Int",     MinimalCost),
    ("Const", "() => Long",    MinimalCost),
    ("Const", "() => BigInt",  MinimalCost),
    ("Const", "() => String",  MinimalCost),
    ("Const", "() => GroupElement", MinimalCost),
    ("Const", "() => SigmaProp", MinimalCost),
    ("Const", "() => Array[IV]", MinimalCost),
    ("Const", "() => Box", MinimalCost),
    ("ConcreteCollection", "() => Array[IV]", MinimalCost),
    ("If", "(Boolean, T, T) => T", MinimalCost),
//    ("If", "(Boolean, Unit, Unit) => Unit", MinimalCost),
//    ("If", "(Boolean, Byte, Byte) => Byte", MinimalCost),
//    ("If", "(Boolean, Short, Short) => Short", MinimalCost),
//    ("If", "(Boolean, Int, Int) => Int", MinimalCost),
//    ("If", "(Boolean, Long, Long) => Long", MinimalCost),
//    ("If", "(Boolean, BigInt, BigInt) => BigInt", MinimalCost),
//    ("If", "(Boolean, GroupElement, GroupElement) => GroupElement", MinimalCost),
//    ("If", "(Boolean, SigmaProp, SigmaProp) => SigmaProp", MinimalCost),
//    ("If", "(Boolean, Array[IV], Array[IV]) => Array[IV]", MinimalCost),
    ("Self$", "Context => Box", MinimalCost),
    ("GroupGenerator$", "() => GroupElement", MinimalCost),
    ("AccessBox", "Context => Box", MinimalCost),
    ("GetVar", "(Context, Byte) => Option[T]", MinimalCost),
    ("AccessRegister", "Box => Option[T]", MinimalCost),
    ("ExtractAmount", "(Box) => Long", MinimalCost),
    ("ExtractId", "(Box) => Array[Byte]", MinimalCost),
    ("ExtractBytes", "(Box) => Array[Byte]", MinimalCost),
    ("ExtractScriptBytes", "(Box) => Array[Byte]", MinimalCost),
    ("ExtractBytesWithNoRef", "(Box) => Array[Byte]", MinimalCost),
    ("ExtractRegisterAs", "(Box,Byte) => Array[BigInt]", MinimalCost),

    ("Exponentiate", "(GroupElement,BigInt) => GroupElement", MinimalCost),
    ("MultiplyGroup", "(GroupElement,GroupElement) => GroupElement", MinimalCost),
    ("ByteArrayToBigInt", "(Array[Byte]) => BigInt", MinimalCost),
    ("new_BigInteger_per_item", "(Array[Byte]) => BigInt", MinimalCost),

    ("Slice", "(Array[IV],Int,Int) => Array[IV]", MinimalCost),
    ("Append", "(Array[IV],Array[IV]) => Array[IV]", MinimalCost),
    ("SizeOf", "(Array[IV]) => Int", MinimalCost),
    ("ByIndex", "(Array[IV],Int) => IV", MinimalCost),

    ("SigmaPropIsValid", "SigmaProp => Boolean", MinimalCost),
    ("SigmaPropBytes", "SigmaProp => Array[Byte]", MinimalCost),
    ("BinAnd", "(Boolean, Boolean) => Boolean", MinimalCost),
    ("BinOr", "(Boolean, Boolean) => Boolean", MinimalCost),
    ("AND", "(Array[Boolean]) => Boolean", MinimalCost),
    ("OR_per_item", "(Array[Boolean]) => Boolean", 0.0001),
    ("AND_per_item", "(Array[Boolean]) => Boolean", MinimalCost),
    ("AtLeast", "(Int, Array[Boolean]) => Boolean", MinimalCost),
    ("CalcBlake2b256_per_kb", "(Array[Byte]) => Array[Byte]", 0.0001),
    ("CalcSha256_per_kb", "(Array[Byte]) => Array[Byte]", 0.0001),
    ("Xor_per_kb", "(Array[Byte],Array[Byte]) => Array[Byte]", MinimalCost),
    ("GT_per_kb", "(T,T) => Boolean", MinimalCost),
    ("GE_per_kb", "(T,T) => Boolean", MinimalCost),
    ("LE_per_kb", "(T,T) => Boolean", MinimalCost),
    ("LT_per_kb", "(T,T) => Boolean", MinimalCost),
    ("EQ_per_kb", "(T,T) => Boolean", MinimalCost),
    ("NEQ_per_kb", "(T,T) => Boolean", MinimalCost),
    ("GT", "(BigInt,BigInt) => Boolean", 0.0001),
    (">_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),
    ("+", "(Byte, Byte) => Byte", 0.0001),
    ("+", "(Short, Short) => Short", 0.0001),
    ("+", "(Int, Int) => Int", 0.0001),
    ("+", "(Long, Long) => Long", 0.0001),

    ("-", "(Byte, Byte) => Byte", 0.0001),
    ("-", "(Short, Short) => Short", 0.0001),
    ("-", "(Int, Int) => Int", 0.0001),
    ("-", "(Long, Long) => Long", 0.0001),

    ("*", "(Byte, Byte) => Byte", 0.0001),
    ("*", "(Short, Short) => Short", 0.0001),
    ("*", "(Int, Int) => Int", 0.0001),
    ("*", "(Long, Long) => Long", 0.0001),

    ("/", "(Byte, Byte) => Byte", 0.0001),
    ("/", "(Short, Short) => Short", 0.0001),
    ("/", "(Int, Int) => Int", 0.0001),
    ("/", "(Long, Long) => Long", 0.0001),

    ("%", "(Byte, Byte) => Byte", 0.0001),
    ("%", "(Short, Short) => Short", 0.0001),
    ("%", "(Int, Int) => Int", 0.0001),
    ("%", "(Long, Long) => Long", 0.0001),

    ("+", "(BigInt, BigInt) => BigInt", 0.0001),
    ("+_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),

    ("-", "(BigInt, BigInt) => BigInt", 0.0001),
    ("-_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),

    ("*", "(BigInt, BigInt) => BigInt", 0.0001),
    ("*_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),
    
    ("/", "(BigInt, BigInt) => BigInt", 0.0001),
    ("/_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),

    ("%", "(BigInt, BigInt) => BigInt", 0.0001),
    ("%_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),

    ("Downcast", s"(${Downcast.tT}) => ${Downcast.tR}", MinimalCost),

    ("Upcast", s"(${Upcast.tT}) => ${Upcast.tR}", MinimalCost),

    ("min", "(Byte, Byte) => Byte", MinimalCost),
    ("min", "(Short, Short) => Short", MinimalCost),
    ("min", "(Int, Int) => Int", MinimalCost),
    ("min", "(Long, Long) => Long", MinimalCost),
    ("min", "(BigInt, BigInt) => BigInt", MinimalCost),
    ("min_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),

    ("max", "(Byte, Byte) => Byte", MinimalCost),
    ("max", "(Short, Short) => Short", MinimalCost),
    ("max", "(Int, Int) => Int", MinimalCost),
    ("max", "(Long, Long) => Long", MinimalCost),
    ("max", "(BigInt, BigInt) => BigInt", MinimalCost),
    ("max_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),

    ("TreeModifications", "(AvlTree, Array[Byte], Array[Byte]) => Option[Array[Byte]]", MinimalCost),
    ("TreeLookup", "(AvlTree, Array[Byte], Array[Byte]) => Option[Array[Byte]]", MinimalCost),
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

    val AndDeclaration = 1
    val AndPerChild = 1

    val OrDeclaration = 1
    val OrPerChild = 1

    val BinOrDeclaration = 1
    val BinAndDeclaration = 1
    val IfDeclaration = 1

    /**PropLeaf declaration cost, wrapped script cost to be added as well.*/
    val AtLeastDeclaration = 1
    val AtLeastPerChild = 1

    //PropLeaf declaration cost, wrapped script cost to be added as well.
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

    val OptionGet = 1
    val OptionGetOrElse = 1
    val OptionIsDefined = 1
  }
}
