package sigmastate.utxo

import org.ergoplatform.SigmaConstants
import sigmastate._
import sigmastate.interpreter.Interpreter
import sigmastate.lang.SigmaParser
import sigmastate.lang.Terms.OperationId

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
      case Some(cost) =>
        // println(s"$operId -> $cost")
        cost
      case None => //costToInt(MinimalCost)
        sys.error(s"Cannot find cost in CostTable for $operId")
    }
  }
}

object CostTable {
  type ExpressionCost = Int

  val MinimalCost = 10

  val interpreterInitCost = 10000
  val perGraphNodeCost = 200

  /** Scaling factors to be applied to estimated cost (totalCost = cost * costFactorIncrease / costFactorDecrease */
  val costFactorIncrease: Int = 1
  val costFactorDecrease: Int = 1

  val expCost = 5000
  val multiplyGroup = 50
  val negateGroup = 50
  val groupElementConst = 1
  val decodePointCost = 1
  val constCost = 10
  val lambdaCost = 10

  /** Cost of creating new instances (kind of memory allocation cost).
    * When the instance already exists them the corresponding Access/Extract cost should be added.
    */
  val newPrimValueCost = 1
  val newCollValueCost = 1
  val newPairValueCost = 1
  val newOptionValueCost = 1
  val newAvlTreeCost = 10

  val plusMinus = 10
  val multiply = 10

  val plusMinusBigInt = 10
  val comparisonBigInt = 10
  val multiplyBigInt = 50
  val newBigIntPerItem = 1

  val hashPerKb = 100

  val avlTreeOp = hashPerKb * 2

  val collAccess = 5
  val collLength  = 5 // TODO costing: should be >= selectField
  val collByIndex = 5 // TODO costing: should be >= selectField

  val collToColl = 20
  val lambdaInvoke = 30  // interpreter overhead on each lambda invocation (map, filter, forall, etc)
  val concreteCollectionItemCost = 10  // since each item is a separate graph node
  val comparisonCost = 10
  val comparisonPerKbCost = 10

  val logicCost = 10

  val sigmaAndCost = 10
  val sigmaOrCost = 40

  val proveDlogEvalCost = groupElementConst + constCost + 2 * expCost + multiplyGroup
  val proveDHTupleEvalCost = proveDlogEvalCost * 4  // we approximate it as multiple of proveDlogEvalCost

  val castOp = 10  // should be >= selectField
  val castOpBigInt = 40

  val treeOp = 1000

  val getVarCost       = 20
  val extractCost      = 10
  val selectField      = 10
  val accessBox        = 10
  val accessRegister   = 10

  val OptionOp         = 10

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

    ("ConcreteCollection", "() => Coll[IV]", collToColl),
    ("GroupGenerator$", "() => GroupElement", constCost),
    ("Self$", "Context => Box", constCost),
    ("AccessAvlTree", "Context => AvlTree", constCost),

    ("SelectField", "() => Unit", selectField),
    ("AccessKiloByteOfData", "() => Unit", extractCost),
    ("AccessBox", "Context => Box", accessBox),
    ("GetVar", "(Context, Byte) => Option[T]", getVarCost),
    ("GetRegister", "(Box, Byte) => Option[T]", accessRegister),
    ("AccessRegister", "Box => Option[T]", accessRegister),
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
    ("new_BigInteger_per_item", "(Coll[Byte]) => BigInt", newBigIntPerItem),
    ("SGroupElement$.negate", "(GroupElement) => GroupElement", negateGroup),

    ("Slice", "(Coll[IV],Int,Int) => Coll[IV]", collToColl),
    ("Append", "(Coll[IV],Coll[IV]) => Coll[IV]", collToColl),

    ("SizeOf", "(Coll[IV]) => Int", collLength),
    ("ByIndex", "(Coll[IV],Int) => IV", collByIndex),
    ("SCollection$.exists", "(Coll[IV],(IV) => Boolean) => Boolean", collToColl),
    ("SCollection$.forall", "(Coll[IV],(IV) => Boolean) => Boolean", collToColl),
    ("SCollection$.map", "(Coll[IV],(IV) => OV) => Coll[OV]", collToColl),
    ("SCollection$.flatMap", "(Coll[IV],(IV) => Coll[OV]) => Coll[OV]", collToColl),
    ("SCollection$.indexOf_per_kb", "(Coll[IV],IV,Int) => Int", collToColl),
    ("SCollection$.zip", "(Coll[IV],Coll[OV]) => Coll[(IV,OV)]", collToColl),
    ("SCollection$.patch", "(Coll[IV],Int,Coll[IV],Int) => Coll[IV]", collToColl),
    ("SCollection$.updated", "(Coll[IV],Int,IV) => Coll[IV]", collToColl),
    ("SCollection$.updateMany_per_kb", "(Coll[IV],Coll[Int],Coll[IV]) => Coll[IV]", collToColl),
    ("SCollection$.filter", "(Coll[IV],(IV) => Boolean) => Coll[IV]", collToColl),

    ("If", "(Boolean, T, T) => T", logicCost),

    ("SigmaPropIsProven", "SigmaProp => Boolean", logicCost),
    ("BoolToSigmaProp", "Boolean => SigmaProp", logicCost),
    ("SigmaPropBytes", "SigmaProp => Coll[Byte]", logicCost),
    ("BinAnd", "(Boolean, Boolean) => Boolean", logicCost),
    ("BinOr", "(Boolean, Boolean) => Boolean", logicCost),
    ("BinXor", "(Boolean, Boolean) => Boolean", logicCost),
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

    ("GT_per_kb", "(T,T) => Boolean", comparisonPerKbCost),
    ("GE_per_kb", "(T,T) => Boolean", comparisonPerKbCost),
    ("LE_per_kb", "(T,T) => Boolean", comparisonPerKbCost),
    ("LT_per_kb", "(T,T) => Boolean", comparisonPerKbCost),
    ("EQ_per_kb", "(T,T) => Boolean", comparisonPerKbCost),
    ("NEQ_per_kb", "(T,T) => Boolean", comparisonPerKbCost),

    ("GT", "(BigInt,BigInt) => Boolean", comparisonBigInt),
    ("GE", "(BigInt,BigInt) => Boolean", comparisonBigInt),
    ("LE", "(BigInt,BigInt) => Boolean", comparisonBigInt),
    ("LT", "(BigInt,BigInt) => Boolean", comparisonBigInt),
    ("EQ", "(BigInt,BigInt) => Boolean", comparisonBigInt),
    ("NEQ", "(BigInt,BigInt) => Boolean", comparisonBigInt),
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
    ("ModQArithOp", "(BigInt, BigInt) => BigInt", MinimalCost),

    ("Downcast", s"(${Downcast.tT}) => ${Downcast.tR}", castOp),
    ("Upcast", s"(${Upcast.tT}) => ${Upcast.tR}", castOp),

    ("Downcast", s"(BigInt) => ${Downcast.tR}", castOpBigInt),
    ("Upcast", s"(${Upcast.tT}) => BigInt", castOpBigInt),

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

    ("SAvlTree$.insert_per_kb", "(AvlTree, Coll[(Coll[Byte], Coll[Byte])], Coll[Byte]) => Option[AvlTree]", avlTreeOp),
    ("SAvlTree$.update_per_kb", "(AvlTree, Coll[(Coll[Byte], Coll[Byte])], Coll[Byte]) => Option[AvlTree]", avlTreeOp),
    ("SAvlTree$.remove_per_kb", "(AvlTree, Coll[Coll[Byte]], Coll[Byte]) => Option[AvlTree]", avlTreeOp),
    ("SAvlTree$.contains_per_kb", "(AvlTree,Coll[Byte],Coll[Byte]) => Boolean", avlTreeOp),
    ("SAvlTree$.get_per_kb", "(AvlTree,Coll[Byte],Coll[Byte]) => Option[Coll[Byte]]", avlTreeOp),
    ("SAvlTree$.getMany_per_kb", "(AvlTree,Coll[Coll[Byte]],Coll[Byte]) => Coll[Option[Coll[Byte]]]", avlTreeOp),
    ("SAvlTree$.updateDigest", "(AvlTree,Coll[Byte]) => AvlTree", newAvlTreeCost),
    ("SAvlTree$.updateOperations", "(AvlTree,Byte) => AvlTree", newAvlTreeCost),

    ("LongToByteArray", "(Long) => Coll[Byte]", castOp),
    ("ByteArrayToLong", "(Coll[Byte]) => Long", castOp),

    ("ProveDlogEval", "(Unit) => SigmaProp", proveDlogEvalCost),
    ("ProveDHTuple", "(Unit) => SigmaProp", proveDHTupleEvalCost),

    ("SigmaAnd_per_item", "(Coll[SigmaProp]) => SigmaProp", sigmaAndCost),
    ("SigmaOr_per_item", "(Coll[SigmaProp]) => SigmaProp", sigmaOrCost),

    ("SubstConstants_per_kb", "(Coll[Byte], Coll[Int], Coll[T]) => Coll[Byte]", MinimalCost),

    ("DecodePoint", "(Coll[Byte]) => GroupElement", decodePointCost),

    ("SOption$.map", "(Option[T],(T) => R) => Option[R]", OptionOp),
    ("SOption$.filter", "(Option[T],(T) => Boolean) => Option[T]", OptionOp)
  ))

  def fromSeq(items: Seq[(String, String, Int)]): CostTable = {
    val parsed = for ((name, ts, cost) <- items) yield {
      val ty = SigmaParser.parseType(ts).asFunc
      (OperationId(name, ty), cost)
    }
    CostTable(parsed.toMap)
  }

  //Maximum cost of a script
  val ScriptLimit = SigmaConstants.ScriptCostLimit.value

  //Maximum number of expressions in initial(non-reduced script)
  val MaxExpressions = 300

  object CostOf {
    def Constant(constType: SType): Int = constType match {
      case SUnit | SBoolean | SByte | SShort | SInt | SLong | SBigInt | SString |
           SGroupElement | SSigmaProp | SBox | SAvlTree => constCost
      case _: SCollectionType[_] => constCost
      case _ => Interpreter.error(s"Cost is not defined: unexpected constant type $constType")
    }

    /** Cost of: accessing Constant in array by index. */
    def ConstantPlaceholder = 1

    /** Cost of: 1) switch on the number of args 2) allocating a new Scala closure
      * Old cost: ("Lambda", "() => (D1) => R", lambdaCost),*/
    def FuncValue = 2 // cf. lambdaCost

    /** Cost of: 1) switch on the number of args 2) Scala method call 3) add args to env
      * Old cost: lambdaInvoke == 30 */
    def Apply = 20

    /** Cost of: 1) Calling Option.get Scala method. */
    def OptionGet = 2 // cf. selectField

    /** Cost of: 1) Calling Context.OUTPUTS Scala method. */
    def Outputs = 2 // cf. selectField

    /** Cost of: 1) Lookup in immutable HashMap by valId: Int 2) alloc of Some(v) */
    def ValUse = 5


    /** Cost of: allocating new collection
      * @see ConcreteCollection_PerItem */
    def ConcreteCollection = 1 // cf. collToColl

    /** Cost of: one iteration of the loop over items
      * @see ConcreteCollection */
    def ConcreteCollection_PerItem = 1 // cf. logicCost

    //    ("ConcreteCollection", "() => Coll[IV]", collToColl),
//    ("GroupGenerator$", "() => GroupElement", constCost),
//    ("Self$", "Context => Box", constCost),
//    ("AccessAvlTree", "Context => AvlTree", constCost),

    /** Cost of: 1) Calling Tuple2.{_1, _2} Scala methods.
      * Old cost: ("SelectField", "() => Unit", selectField) */
    def SelectField = 2 // cf. selectField

//    ("AccessKiloByteOfData", "() => Unit", extractCost),
//    ("AccessBox", "Context => Box", accessBox),

    /** Cost of: 1) accessing to array of context vars by index
      * Old cost: ("GetVar", "(Context, Byte) => Option[T]", getVarCost) */
    def GetVar = 5

//    ("GetRegister", "(Box, Byte) => Option[T]", accessRegister),
//    ("AccessRegister", "Box => Option[T]", accessRegister),
//    ("ExtractAmount", "(Box) => Long", extractCost),
//    ("ExtractId", "(Box) => Coll[Byte]", extractCost),
//    ("ExtractBytes", "(Box) => Coll[Byte]", extractCost),
//    ("ExtractScriptBytes", "(Box) => Coll[Byte]", extractCost),
//    ("ExtractBytesWithNoRef", "(Box) => Coll[Byte]", extractCost),
//    ("ExtractRegisterAs", "(Box,Byte) => Coll[BigInt]", extractCost),
//    ("SBox$.tokens", "(Box) => Coll[(Coll[Byte],Long)]", extractCost),
//
//    ("Exponentiate", "(GroupElement,BigInt) => GroupElement", expCost),
//    ("MultiplyGroup", "(GroupElement,GroupElement) => GroupElement", multiplyGroup),
//    ("ByteArrayToBigInt", "(Coll[Byte]) => BigInt", castOp),
//    ("new_BigInteger_per_item", "(Coll[Byte]) => BigInt", newBigIntPerItem),
//    ("SGroupElement$.negate", "(GroupElement) => GroupElement", negateGroup),
//
//    ("Slice", "(Coll[IV],Int,Int) => Coll[IV]", collToColl),
//    ("Append", "(Coll[IV],Coll[IV]) => Coll[IV]", collToColl),
//

    /** Cost of: 1) calling Coll.length method (guaranteed to be O(1))
      * Twice the cost of SelectField.
      * Old cost: ("SizeOf", "(Coll[IV]) => Int", collLength) */
    def SizeOf = 4  // cf. collLength

//    ("ByIndex", "(Coll[IV],Int) => IV", collByIndex),
//    ("SCollection$.exists", "(Coll[IV],(IV) => Boolean) => Boolean", collToColl),
//    ("SCollection$.forall", "(Coll[IV],(IV) => Boolean) => Boolean", collToColl),
//    ("SCollection$.map", "(Coll[IV],(IV) => OV) => Coll[OV]", collToColl),
//    ("SCollection$.flatMap", "(Coll[IV],(IV) => Coll[OV]) => Coll[OV]", collToColl),
//    ("SCollection$.indexOf_per_kb", "(Coll[IV],IV,Int) => Int", collToColl),
//    ("SCollection$.zip", "(Coll[IV],Coll[OV]) => Coll[(IV,OV)]", collToColl),
//    ("SCollection$.patch", "(Coll[IV],Int,Coll[IV],Int) => Coll[IV]", collToColl),
//    ("SCollection$.updated", "(Coll[IV],Int,IV) => Coll[IV]", collToColl),
//    ("SCollection$.updateMany_per_kb", "(Coll[IV],Coll[Int],Coll[IV]) => Coll[IV]", collToColl),
//    ("SCollection$.filter", "(Coll[IV],(IV) => Boolean) => Coll[IV]", collToColl),
//
//    ("If", "(Boolean, T, T) => T", logicCost),
//
//    ("SigmaPropIsProven", "SigmaProp => Boolean", logicCost),
//    ("BoolToSigmaProp", "Boolean => SigmaProp", logicCost),
//    ("SigmaPropBytes", "SigmaProp => Coll[Byte]", logicCost),

    /** Cost of: scala `&&` operation
      * Old cost: ("BinAnd", "(Boolean, Boolean) => Boolean", logicCost) */
    def BinAnd = 5 // cf. logicCost

    /** Cost of: scala `||` operation
      * Old cost: ("BinOr", "(Boolean, Boolean) => Boolean", logicCost) */
    def BinOr = 5 // cf. logicCost

    /** Cost of: scala `^` operation
      * Old cost: ("BinXor", "(Boolean, Boolean) => Boolean", logicCost) */
    def BinXor = 5 // cf. logicCost

//    ("AND", "(Coll[Boolean]) => Boolean", logicCost),
//    ("OR_per_item", "(Coll[Boolean]) => Boolean", logicCost),
//    ("AND_per_item", "(Coll[Boolean]) => Boolean", logicCost),
    /** Cost of: operations factored out of reduction loop.
      * @see OR_PerItem,  */
    def OR = 2
    
    /** Cost of: single scala `||` operation amortized over a loop of boolean values.
      * @see OR */
    def OR_PerItem = 2

    /** Cost of: operations factored out of reduction loop.
      * @see BinAnd */
    def AND = 2

    /** Cost of: single scala `&&` operation amortized over a loop of boolean values.
      * @see OR */
    def AND_PerItem = 2

    /** Cost of: operations factored out of reduction loop.
      * @see XOR_PerItem,  */
    def XOR = 2

    /** Cost of: single scala `^` operation amortized over a loop of boolean values.
      * @see XOR */
    def XOR_PerItem = 2

    /** Cost of: constructing new CSigmaProp and allocation collection
      * @see SigmaAnd_PerItem */
    def SigmaAnd = 2 // cf. logicCost

    /** Cost of: one iteration over collection of items
      * @see SigmaAnd */
    def SigmaAnd_PerItem = 1 // cf. logicCost

    /** Cost of: constructing new CSigmaProp and allocation collection
      * @see SigmaOr_PerItem */
    def SigmaOr = 2 // cf. logicCost

    /** Cost of: one iteration over collection of items
      * @see SigmaOr */
    def SigmaOr_PerItem = 1 // cf. logicCost

    /** Cost of: constructing new CSigmaProp value
      * @see AtLeast_PerItem */
    def AtLeast = 2 // cf. logicCost

    /** Cost of: obtaining SigmaBoolean for each item in AtLeast
      * @see AtLeast */
    def AtLeast_PerItem = 1 // cf. logicCost

    /** Cost of: of hashing 1 KiB of data */
    def CalcBlake2b256_PerKb = 100 // cf. hashPerKb

    /** Cost of: of hashing 1 KiB of data */
    def CalcSha256_PerKb = 100 // cf. hashPerKb

//    ("Xor_per_kb", "(Coll[Byte],Coll[Byte]) => Coll[Byte]", hashPerKb / 2),
//    ("XorOf_per_item", "(Coll[Boolean]) => Boolean", logicCost),
//    ("LogicalNot", "(Boolean) => Boolean", logicCost),

    /** Cost of:
      * 1) compute isConstSize for left argument
      * 2) dataSizeOf for left and right arguments
      * Old cost: ("EQ", "(T,T) => Boolean", comparisonCost)
      */
    def EQConstSize = comparisonCost

    /** Cost of:
      * 1) compute isConstSize for left argument
      * 2) dataSizeOf for left and right arguments
      * 3) perform comparison of two arguments
      */
    def EQDynSize(dataSize: Int) = {
      val numKbs = dataSize / 1024 + 1
      numKbs * comparisonPerKbCost
    }

//    ("GT_per_kb", "(T,T) => Boolean", comparisonPerKbCost),
//    ("GE_per_kb", "(T,T) => Boolean", comparisonPerKbCost),
//    ("LE_per_kb", "(T,T) => Boolean", comparisonPerKbCost),
//    ("LT_per_kb", "(T,T) => Boolean", comparisonPerKbCost),
//    ("NEQ_per_kb", "(T,T) => Boolean", comparisonPerKbCost),
//    //    (">_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),

    /** Cost of:
      * 1) resolving ArithOpCompanion by typeCode
      * 2) calling method of Numeric
      */
    def GT(argTpe: SType) = argTpe match {
      case SBigInt => 10 // cf. comparisonBigInt
      case _ => 5 // cf. comparisonCost
    }

    /** Cost of:
      * 1) resolving ArithOpCompanion by typeCode
      * 2) calling method of Numeric
      */
    def GE(argTpe: SType) = argTpe match {
      case SBigInt => 10 // cf. comparisonBigInt
      case _ => 5 // cf. comparisonCost
    }

    /** Cost of:
      * 1) resolving ArithOpCompanion by typeCode
      * 2) calling method of Numeric
      */
    def LT(argTpe: SType) = argTpe match {
      case SBigInt => 10 // cf. comparisonBigInt
      case _ => 5 // cf. comparisonCost
    }

    /** Cost of:
      * 1) resolving ArithOpCompanion by typeCode
      * 2) calling method of Numeric
      */
    def LE(argTpe: SType) = argTpe match {
      case SBigInt => 10 // cf. comparisonBigInt
      case _ => 5 // cf. comparisonCost
    }

    /** Cost of:
      * 1) resolving ArithOpCompanion by typeCode
      * 2) calling method of Numeric
      */
    def Plus(argTpe: SType) = argTpe match {
      case SBigInt => 10 // cf. plusMinusBigInt
      case _ => 5 // cf. plusMinus
    }

    /** Cost of:
      * 1) resolving ArithOpCompanion by typeCode
      * 2) calling method of Numeric
      */
    def Minus(argTpe: SType) = argTpe match {
      case SBigInt => 10 // cf. plusMinusBigInt
      case _ => 5 // cf. plusMinus
    }

    /** Cost of:
      * 1) resolving ArithOpCompanion by typeCode
      * 2) calling method of Numeric
      */
    def Multiply(argTpe: SType) = argTpe match {
      case SBigInt => 50 // cf. multiplyBigInt
      case _ => 5 // cf. multiply
    }

    /** Cost of:
      * 1) resolving ArithOpCompanion by typeCode
      * 2) calling method of Integral
      */
    def Division(argTpe: SType) = argTpe match {
      case SBigInt => 50 // cf. multiplyBigInt
      case _ => 5 // cf. multiply
    }

    /** Cost of:
      * 1) resolving ArithOpCompanion by typeCode
      * 2) calling method of Integral
      */
    def Modulo(argTpe: SType) = argTpe match {
      case SBigInt => 50 // cf. multiplyBigInt
      case _ => 5 // cf. multiply
    }

    /** Cost of:
      * 1) resolving ArithOpCompanion by typeCode
      * 2) calling method of ExactOrdering
      */
    def Min(argTpe: SType) = argTpe match {
      case SBigInt => 10 // cf. comparisonCost
      case _ => 5 // cf. logicCost
    }

    /** Cost of:
      * 1) resolving ArithOpCompanion by typeCode
      * 2) calling method of ExactOrdering
      */
    def Max(argTpe: SType) = argTpe match {
      case SBigInt => 10 // cf. comparisonCost
      case _ => 5 // cf. logicCost
    }

    //    ("Negation", "(Byte) => Byte", MinimalCost),
//    ("Negation", "(Short) => Short", MinimalCost),
//    ("Negation", "(Int) => Int", MinimalCost),
//    ("Negation", "(Long) => Long", MinimalCost),
//    ("Negation", "(BigInt) => BigInt", MinimalCost),
//
//    ("+_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),
//
//    ("-_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),
//
//    ("*_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),
//
//    ("/_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),
//
//    ("%", "(BigInt, BigInt) => BigInt", multiplyBigInt),
//    ("%_per_item", "(BigInt, BigInt) => BigInt", MinimalCost),
//
//    ("ModQ", "(BigInt) => BigInt", MinimalCost),
//    ("ModQArithOp", "(BigInt, BigInt) => BigInt", MinimalCost),
//
//    ("Downcast", s"(${Downcast.tT}) => ${Downcast.tR}", castOp),
//    ("Upcast", s"(${Upcast.tT}) => ${Upcast.tR}", castOp),
//
//    ("Downcast", s"(BigInt) => ${Downcast.tR}", castOpBigInt),
//    ("Upcast", s"(${Upcast.tT}) => BigInt", castOpBigInt),
//
    //    ("min_per_item", "(BigInt, BigInt) => BigInt", comparisonCost),
    //
//    ("max_per_item", "(BigInt, BigInt) => BigInt", comparisonCost),
//
//    ("SAvlTree$.insert_per_kb", "(AvlTree, Coll[(Coll[Byte], Coll[Byte])], Coll[Byte]) => Option[AvlTree]", avlTreeOp),
//    ("SAvlTree$.update_per_kb", "(AvlTree, Coll[(Coll[Byte], Coll[Byte])], Coll[Byte]) => Option[AvlTree]", avlTreeOp),
//    ("SAvlTree$.remove_per_kb", "(AvlTree, Coll[Coll[Byte]], Coll[Byte]) => Option[AvlTree]", avlTreeOp),
//    ("SAvlTree$.contains_per_kb", "(AvlTree,Coll[Byte],Coll[Byte]) => Boolean", avlTreeOp),
//    ("SAvlTree$.get_per_kb", "(AvlTree,Coll[Byte],Coll[Byte]) => Option[Coll[Byte]]", avlTreeOp),
//    ("SAvlTree$.getMany_per_kb", "(AvlTree,Coll[Coll[Byte]],Coll[Byte]) => Coll[Option[Coll[Byte]]]", avlTreeOp),
//    ("SAvlTree$.updateDigest", "(AvlTree,Coll[Byte]) => AvlTree", newAvlTreeCost),
//    ("SAvlTree$.updateOperations", "(AvlTree,Byte) => AvlTree", newAvlTreeCost),
//
//    ("LongToByteArray", "(Long) => Coll[Byte]", castOp),
//    ("ByteArrayToLong", "(Coll[Byte]) => Long", castOp),
//
//    ("ProveDlogEval", "(Unit) => SigmaProp", proveDlogEvalCost),
//    ("ProveDHTuple", "(Unit) => SigmaProp", proveDHTupleEvalCost),
//
//    ("SigmaAnd_per_item", "(Coll[SigmaProp]) => SigmaProp", sigmaAndCost),
//    ("SigmaOr_per_item", "(Coll[SigmaProp]) => SigmaProp", sigmaOrCost),
//
//    ("SubstConstants_per_kb", "(Coll[Byte], Coll[Int], Coll[T]) => Coll[Byte]", MinimalCost),
//
//    ("DecodePoint", "(Coll[Byte]) => GroupElement", decodePointCost),
//
//    ("SOption$.map", "(Option[T],(T) => R) => Option[R]", OptionOp),
//    ("SOption$.filter", "(Option[T],(T) => Boolean) => Option[T]", OptionOp)

  }
}





