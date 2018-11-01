package sigmastate.utxo


object CostTable {
  type ExpressionCost = Int

  //Maximum cost of a script
  val ScriptLimit = 1000000

  //Maximum number of expressions in initial(non-reduced script)
  val MaxExpressions = 300

  object Cost {
    //Cost of multiply two group elements
    val MultiplyGroup = 50

    //Cost of one exponentiation
    val Exponentiate = 3000

    val ByteArrayPerKilobyte = 200

    //Node which holds true/false
    val ConstantNode = 1

    val HeightAccess = 1

    //cost for CustomByteArray declaration. Additional cost to be calculated when data is known
    //(and CustomByteArray being converted to ByteArrayLeaf)
    val ByteArrayDeclaration = 1



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

    val AtLeastDeclaration = 1
    val AtLeastPerChild = 1

    //PropLeaf declaration cost, wrapped script cost to be added as well.
    val PropLeafDeclaration = 500

    //Cost of Blake256 declaration
    val Blake256bDeclaration = 20

    val TxHasOutputDeclaration = 100
    val TxOutputDeclaration = 100

    val OutputScript = 100
    val OutputAmount = 1

    val TxOutBytes = 10

    val SelfVariableDeclaration = 1

    val FoldDeclaration = 200

    val ByIndexDeclaration = 50
    val SelectFieldDeclaration = 50
    val SigmaPropIsValidDeclaration = 50
    val SigmaPropBytes = 50
    val ParseSigmaProp = 50

    val MapDeclaration = 100

    val WhereDeclaration = 200

    val ExistsDeclaration = 200

    val ForAllDeclaration = 200

    val SizeOfDeclaration = 50

    val ConcreteCollection = 20
    val Tuple = 20

    //Checking Shnorr signature is about 2 exponentiations and one multiplication
    val Dlog = 2 * Exponentiate + MultiplyGroup


    val OptionGet = 1
    val OptionGetOrElse = 1
    val OptionIsDefined = 1
  }
}
