package sigmastate.utxo

import sigmastate.{Downcast, Upcast}
import sigmastate.lang.SigmaParser
import sigmastate.lang.Terms.OperationId
import scala.collection.mutable

case class CostTable(operCosts: Map[OperationId, Int]) extends (OperationId => Int) {
  @inline private def cleanOperId(operId: OperationId): OperationId = {
    if (operId.opType.tpeParams.isEmpty) operId
    else operId.copy(opType = operId.opType.copy(tpeParams = Nil))
  }
  @inline final def get(operId: OperationId): Option[Int] = {
    val cleanId = cleanOperId(operId)
    operCosts.get(cleanId)
  }
  override def apply(operId: OperationId): Int = {
    val costOpt = this.get(operId)
    costOpt match {
      case Some(cost) => cost
      case None => //costToInt(MinimalCost)
        sys.error(s"Cannot find cost in CostTable for $operId")
    }
  }
}

object CostTable {
  type ExpressionCost = Int

  val MinimalCost = 1

  val expCost = 5000
  val multiplyGroup = 50
  val negateGroup = 50
  val groupElementConst = 1
  val constCost = 1
  val lambdaCost = 1

  /** Cost of creating new instances (kind of memory allocation cost).
    * When the instance already exists them the corresponding Access/Extract cost should be added.
    */
  val newPrimValueCost = 1
  val newCollValueCost = 1
  val newPairValueCost = 1
  val newOptionValueCost = 1

  val plusMinus = 2
  val multiply = 10

  val plusMinusBigInt = 10
  val multiplyBigInt = 50

  val hashPerKb = 100

  val collAccess = 5

  val collToColl = 20

  val comparisonCost = 3

  val logicCost = 2

  val sigmaAndCost = 10
  val sigmaOrCost = 40

  val castOp = 5

  val treeOp = 1000

  val extractCost = 10

  val DefaultCosts = CostTable.fromSeq(Seq(
    ("Const", "() => Unit",    constCost),
    ("Const", "() => Boolean", constCost),
    ("Const", "() => Byte",    constCost),
    ("Const", "() => Short",   constCost),
    ("Const", "() => Int",     constCost),
    ("Const", "() => Long",    constCost),
    ("Const", "() => BigInt",  constCost),
    ("Const", "() => String",  constCost),
    ("Const", "() => GroupElement", constCost),
    ("Const", "() => SigmaProp", constCost),
    ("Const", "() => Coll[IV]", constCost),
    ("Const", "() => Box", constCost),
    ("Const", "() => AvlTree", constCost),

    ("Lambda", "() => (D1) => R", lambdaCost),

    ("ConcreteCollection", "() => Coll[IV]", constCost),
    ("GroupGenerator$", "() => GroupElement", constCost),
    ("Self$", "Context => Box", constCost),
    ("AccessAvlTree", "Context => AvlTree", constCost),

    ("SelectField", "() => Unit", extractCost),
    ("AccessKiloByteOfData", "() => Unit", extractCost),
    ("AccessBox", "Context => Box", extractCost),
    ("GetVar", "(Context, Byte) => Option[T]", extractCost),
    ("GetRegister", "(Box, Byte) => Option[T]", extractCost),
    ("AccessRegister", "Box => Option[T]", extractCost),
    ("ExtractAmount", "(Box) => Long", extractCost),
    ("ExtractId", "(Box) => Coll[Byte]", extractCost),
    ("ExtractBytes", "(Box) => Coll[Byte]", extractCost),
    ("ExtractScriptBytes", "(Box) => Coll[Byte]", extractCost),
    ("ExtractBytesWithNoRef", "(Box) => Coll[Byte]", extractCost),
    ("ExtractRegisterAs", "(Box,Byte) => Coll[BigInt]", extractCost),
    ("SBox$.tokens", "(Box) => Coll[(Coll[Byte],Long)]", extractCost),

    ("Exponentiate", "(GroupElement,BigInt) => GroupElement", expCost),
    ("MultiplyGroup", "(GroupElement,GroupElement) => GroupElement", multiplyGroup),
    ("ByteArrayToBigInt", "(Coll[Byte]) => BigInt", castOp),
    ("new_BigInteger_per_item", "(Coll[Byte]) => BigInt", MinimalCost),
    ("SGroupElement$.negate", "(GroupElement) => GroupElement", negateGroup),

    ("Slice", "(Coll[IV],Int,Int) => Coll[IV]", collToColl),
    ("Append", "(Coll[IV],Coll[IV]) => Coll[IV]", collToColl),
    ("SizeOf", "(Coll[IV]) => Int", collAccess),
    ("ByIndex", "(Coll[IV],Int) => IV", collAccess),
    ("SCollection$.indexOf_per_kb", "(Coll[IV],IV,Int) => Int", collToColl),
    ("SCollection$.segmentLength", "(Coll[IV],(IV) => Boolean,Int) => Int", collToColl),
    ("SCollection$.indexWhere", "(Coll[IV],(IV) => Boolean,Int) => Int", collToColl),
    ("SCollection$.lastIndexWhere", "(Coll[IV],(IV) => Boolean,Int) => Int", collToColl),
    ("SCollection$.zip", "(Coll[IV],Coll[OV]) => Coll[(IV,OV)]", collToColl),
    ("SCollection$.partition", "(Coll[IV],(IV) => Boolean) => (Coll[IV],Coll[IV])", collToColl),
    ("SCollection$.patch", "(Coll[IV],Int,Coll[IV],Int) => Coll[IV]", collToColl),
    ("SCollection$.updated", "(Coll[IV],Int,IV) => Coll[IV]", collToColl),
    ("SCollection$.updateMany_per_kb", "(Coll[IV],Coll[Int],Coll[IV]) => Coll[IV]", collToColl),

    ("If", "(Boolean, T, T) => T", logicCost),

    ("SigmaPropIsProven", "SigmaProp => Boolean", logicCost),
    ("BoolToSigmaProp", "Boolean => SigmaProp", logicCost),
    ("SigmaPropBytes", "SigmaProp => Coll[Byte]", logicCost),
    ("BinAnd", "(Boolean, Boolean) => Boolean", logicCost),
    ("BinOr", "(Boolean, Boolean) => Boolean", logicCost),
    ("AND", "(Coll[Boolean]) => Boolean", logicCost),
    ("OR_per_item", "(Coll[Boolean]) => Boolean", logicCost),
    ("AND_per_item", "(Coll[Boolean]) => Boolean", logicCost),
    ("AtLeast", "(Int, Coll[Boolean]) => Boolean", logicCost),
    ("CalcBlake2b256_per_kb", "(Coll[Byte]) => Coll[Byte]", hashPerKb),
    ("CalcSha256_per_kb", "(Coll[Byte]) => Coll[Byte]", hashPerKb),
    ("Xor_per_kb", "(Coll[Byte],Coll[Byte]) => Coll[Byte]", hashPerKb / 2),
    ("XorOf_per_item", "(Coll[Boolean]) => Boolean", logicCost),
    ("LogicalNot", "(Boolean) => Boolean", logicCost),

    ("GT", "(T,T) => Boolean", comparisonCost),
    ("GE", "(T,T) => Boolean", comparisonCost),
    ("LE", "(T,T) => Boolean", comparisonCost),
    ("LT", "(T,T) => Boolean", comparisonCost),
    ("EQ", "(T,T) => Boolean", comparisonCost),
    ("NEQ", "(T,T) => Boolean", comparisonCost),

    ("GT_per_kb", "(T,T) => Boolean", comparisonCost),
    ("GE_per_kb", "(T,T) => Boolean", comparisonCost),
    ("LE_per_kb", "(T,T) => Boolean", comparisonCost),
    ("LT_per_kb", "(T,T) => Boolean", comparisonCost),
    ("EQ_per_kb", "(T,T) => Boolean", comparisonCost),
    ("NEQ_per_kb", "(T,T) => Boolean", comparisonCost),

    ("GT", "(BigInt,BigInt) => Boolean", plusMinusBigInt),
    ("GE", "(BigInt,BigInt) => Boolean", plusMinusBigInt),
    ("LE", "(BigInt,BigInt) => Boolean", plusMinusBigInt),
    ("LT", "(BigInt,BigInt) => Boolean", plusMinusBigInt),
    ("EQ", "(BigInt,BigInt) => Boolean", plusMinusBigInt),
    ("NEQ", "(BigInt,BigInt) => Boolean", plusMinusBigInt),
//    (">_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),

    ("+", "(Byte, Byte) => Byte", plusMinus),
    ("+", "(Short, Short) => Short", plusMinus),
    ("+", "(Int, Int) => Int", plusMinus),
    ("+", "(Long, Long) => Long", plusMinus),

    ("-", "(Byte, Byte) => Byte", plusMinus),
    ("-", "(Short, Short) => Short", plusMinus),
    ("-", "(Int, Int) => Int", plusMinus),
    ("-", "(Long, Long) => Long", plusMinus),

    ("*", "(Byte, Byte) => Byte", multiply),
    ("*", "(Short, Short) => Short", multiply),
    ("*", "(Int, Int) => Int", multiply),
    ("*", "(Long, Long) => Long", multiply),

    ("/", "(Byte, Byte) => Byte", multiply),
    ("/", "(Short, Short) => Short", multiply),
    ("/", "(Int, Int) => Int", multiply),
    ("/", "(Long, Long) => Long", multiply),

    ("%", "(Byte, Byte) => Byte", multiply),
    ("%", "(Short, Short) => Short", multiply),
    ("%", "(Int, Int) => Int", multiply),
    ("%", "(Long, Long) => Long", multiply),

    ("Negation", "(Byte) => Byte", MinimalCost),
    ("Negation", "(Short) => Short", MinimalCost),
    ("Negation", "(Int) => Int", MinimalCost),
    ("Negation", "(Long) => Long", MinimalCost),
    ("Negation", "(BigInt) => BigInt", MinimalCost),

    ("+", "(BigInt, BigInt) => BigInt", plusMinusBigInt),
    ("+_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),

    ("-", "(BigInt, BigInt) => BigInt", plusMinusBigInt),
    ("-_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),

    ("*", "(BigInt, BigInt) => BigInt", multiplyBigInt),
    ("*_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),

    ("/", "(BigInt, BigInt) => BigInt", multiplyBigInt),
    ("/_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),

    ("%", "(BigInt, BigInt) => BigInt", multiplyBigInt),
    ("%_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),

    ("ModQ", "(BigInt) => BigInt", MinimalCost),

    ("Downcast", s"(${Downcast.tT}) => ${Downcast.tR}", castOp),
    ("Upcast", s"(${Upcast.tT}) => ${Upcast.tR}", castOp),

    ("min", "(Byte, Byte) => Byte", logicCost),
    ("min", "(Short, Short) => Short", logicCost),
    ("min", "(Int, Int) => Int", logicCost),
    ("min", "(Long, Long) => Long", logicCost),
    ("min", "(BigInt, BigInt) => BigInt", comparisonCost),
    ("min_per_item", "(BigInt, BigInt) => BigInt", comparisonCost),

    ("max", "(Byte, Byte) => Byte", logicCost),
    ("max", "(Short, Short) => Short", logicCost),
    ("max", "(Int, Int) => Int", logicCost),
    ("max", "(Long, Long) => Long", logicCost),
    ("max", "(BigInt, BigInt) => BigInt", comparisonCost),
    ("max_per_item", "(BigInt, BigInt) => BigInt", comparisonCost),

    ("SAvlTree$.insert_per_kb", "(AvlTree, Coll[(Coll[Byte], Coll[Byte])], Coll[Byte]) => Option[AvlTree]", hashPerKb * 2),
    ("SAvlTree$.update_per_kb", "(AvlTree, Coll[(Coll[Byte], Coll[Byte])], Coll[Byte]) => Option[AvlTree]", hashPerKb * 2),
    ("SAvlTree$.remove_per_kb", "(AvlTree, Coll[Coll[Byte]], Coll[Byte]) => Option[AvlTree]", hashPerKb * 2),
    ("SAvlTree$.contains_per_kb", "(AvlTree,Coll[Byte],Coll[Byte]) => Boolean", hashPerKb * 2),
    ("SAvlTree$.get_per_kb", "(AvlTree,Coll[Byte],Coll[Byte]) => Option[Coll[Byte]]", hashPerKb * 2),
    ("SAvlTree$.getMany_per_kb", "(AvlTree,Coll[Coll[Byte]],Coll[Byte]) => Coll[Option[Coll[Byte]]]", hashPerKb * 2),

    ("LongToByteArray", "(Long) => Coll[Byte]", castOp),
    ("ByteArrayToLong", "(Coll[Byte]) => Long", castOp),

    ("ProveDlogEval", "(Unit) => SigmaProp", groupElementConst + constCost + 2 * expCost + multiplyGroup),

    //cost if of twice prove dlog
    ("ProveDHTuple", "(Unit) => SigmaProp", 2 * (groupElementConst + constCost + 2 * expCost + multiplyGroup)),

    ("SigmaAnd_per_item", "(Coll[SigmaProp]) => SigmaProp", sigmaAndCost),
    ("SigmaOr_per_item", "(Coll[SigmaProp]) => SigmaProp", sigmaOrCost),

    ("SubstConstants_per_kb", "(Coll[Byte], Coll[Int], Coll[T]) => Coll[Byte]", MinimalCost),

    ("DecodePoint", "(Coll[Byte]) => GroupElement", MinimalCost),
  ))

  def fromSeq(items: Seq[(String, String, Int)]): CostTable = {
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
    val SigmaPropIsProvenDeclaration = 50
    val SigmaPropBytesDeclaration = 50

    val MapDeclaration = 100
    val FilterDeclaration = 200
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

object CostTableStat {
  // NOTE: make immutable before making public
  private class StatItem(
    /** How many times the operation has been executed */
    var count: Long,
    /** Sum of all execution times */
    var sum: Long,
    /** Minimal length of the collection produced by the operation */
    var minLen: Int,
    /** Maximal length of the collection produced by the operation */
    var maxLen: Int,
    /** Sum of all lengths of the collections produced by the operation */
    var sumLen: Int
  )
  private val stat = mutable.HashMap[OperationId, StatItem]()
  def addOpTime(op: OperationId, time: Long, len: Int) = {
    stat.get(op) match {
      case Some(item) =>
        item.count += 1
        item.sum += time
        item.minLen = item.minLen min len
        item.maxLen = item.maxLen max len
        item.sumLen += len
      case None =>
        stat(op) = new StatItem(1, time, minLen = len, maxLen = len, sumLen = len)
    }
  }

  /** Prints the following string
    * Seq(
    * ("Const", "() => SByte", 1206), // count=199
    * ("GT", "(T,T) => SBoolean", 7954), // count=157
    * ("/", "(SByte,SByte) => SByte", 25180), // count=2
    * ("Inputs$", "(SContext) => Coll[SBox]", 4699), // count=443; minLen=0; maxLen=1000; avgLen=9
    * ("OptionIsDefined", "(Option[SSigmaProp]) => SBoolean", 9251), // count=2
    * )
    * */
  def costTableString: String = {
    stat.map { case (opId, item) =>
      val cost = item.sum / item.count
      val avgLen = item.sumLen / item.count
      val isColl = opId.opType.tRange.isCollection
      "\n" + s"""("${opId.name}", "${opId.opType}", $cost), // count=${item.count}${if (isColl) s"; minLen=${item.minLen}; maxLen=${item.maxLen}; avgLen=$avgLen" else ""}"""
    }.mkString("Seq(", "", "\n)")
  }
}

