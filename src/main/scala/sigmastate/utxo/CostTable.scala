package sigmastate.utxo


object CostTable {
  type ExpressionCost = Int

  //Maximum cost of a script
  val ScriptLimit = 1000000

  //Maximum number of expressions in initial(non-reduced script)
  val MaxExpressions = 300

  object Cost {
    //Node which holds true/false
    val ConstantNode = 1

    val HeightAccess = 1

    //cost for CustomByteArray declaration. Additional cost to be calculated when data is known
    //(and CustomByteArray being converted to ByteArrayLeaf)
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
    val GroupElementConstantDeclaration = 10
    val BoxConstantDeclaration = 10
    val AvlTreeConstantDeclaration = 50

    val AndDeclaration = 10
    val AndPerChild = 1


    val OrDeclaration = 1
    val OrPerChild = 1

    //PropLeaf declaration cost, wrapped script cost to be added as well.
    val PropLeafDeclaration = 500

    //Cost of Blake256 declaration
    val Blake256bDeclaration = 20

    val Dlog = 10000

    val TxHasOutputDeclaration = 100
    val TxOutputDeclaration = 100

    val OutputScript = 100
    val OutputAmount = 1

    val TxOutBytes = 10

    val SelfVariableDeclaration = 1

    val FoldDeclaration = 200

    val ByIndexDeclaration = 50
    val SelectFieldDeclaration = 50

    val MapDeclaration = 100

    val WhereDeclaration = 200

    val ExistsDeclaration = 200

    val ForAllDeclaration = 200

    val SizeOfDeclaration = 50

    val ConcreteCollection = 20

    val Exponentiate = 5000

    val MultiplyGroup = 50
  }
}
