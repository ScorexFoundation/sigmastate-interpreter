package sigmastate.serialization

import sigmastate.Values.{ErgoTree, IntConstant, SigmaPropValue}
import sigmastate._
import sigmastate.eval.IRContext
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.lang.exceptions.{InputSizeLimitExceeded, SerializerException}
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer

class ErgoTreeSerializerSpecification extends SerializationSpecification
  with SigmaTestingCommons {

  implicit lazy val IR: TestingIRContext = new TestingIRContext {
    beginPass(noConstPropagationPass)
  }

  private def extractConstants(tree: SigmaPropValue)(implicit IR: IRContext): ErgoTree = {
    import ErgoTree._
    val env = Map[String, Any]()
    val IR.Pair(calcF, _) = IR.doCosting(env, tree)
    val extractConstants = new ConstantStore()
    val outTree = IR.buildTree(calcF, Some(extractConstants))
    val constants = extractConstants.getAll
    val header = if (constants.isEmpty) DefaultHeader else ConstantSegregationHeader
    val ergoTree = ErgoTree(header, constants, outTree)
    ergoTree
  }

  property("(de)serialization round trip using treeBytes()") {
    val tree = EQ(Plus(10, 20), IntConstant(30)).toSigmaProp
    val ergoTree = extractConstants(tree)
    val bytes = DefaultSerializer.serializeErgoTree(ergoTree)
    val (_, _, deserializedConstants, treeBytes) = DefaultSerializer
      .deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes))
    deserializedConstants shouldEqual ergoTree.constants
    val r = SigmaSerializer.startReader(treeBytes, new ConstantStore(deserializedConstants),
      resolvePlaceholdersToConstants = true)
    val deserializedTree = ValueSerializer.deserialize(r)
    deserializedTree shouldEqual tree
  }

  property("Constant extraction via compiler pass: (de)serialization round trip") {
    val prop = EQ(Plus(10, 20), IntConstant(30)).toSigmaProp
    val ergoTree = extractConstants(prop)
    val bytes = DefaultSerializer.serializeErgoTree(ergoTree)
    val deserializedTree = DefaultSerializer.deserializeErgoTree(bytes)
    deserializedTree shouldEqual ergoTree
  }

  property("failed type check on tree deserialization") {
    forAll(numExprTreeNodeGen) { numProp =>
      val bytes = DefaultSerializer.serializeErgoTree(extractConstants(numProp.asInstanceOf[SigmaPropValue]))
      an[SerializerException] should be thrownBy DefaultSerializer.deserializeErgoTree(bytes)
      an[SerializerException] should be thrownBy DefaultSerializer.deserializeErgoTree(bytes)
    }
  }

  property("Constant extraction during serialization: (de)serialization round trip") {
    val tree = EQ(Plus(10, 20), IntConstant(30)).toSigmaProp.treeWithSegregation
    val bytes = DefaultSerializer.serializeErgoTree(tree)
    val (_, _, deserializedConstants, _) = DefaultSerializer.
      deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes))
    deserializedConstants.length shouldBe 3
    val deserializedTree = DefaultSerializer.deserializeErgoTree(bytes)
    deserializedTree shouldEqual tree
  }

  property("tree with placeholders bytes should be equal if only constants are different") {
    val tree1 = EQ(Plus(10, 20), IntConstant(30)).toSigmaProp.treeWithSegregation
    val tree2 = EQ(Plus(30, 40), IntConstant(70)).toSigmaProp.treeWithSegregation
    val bytes1 = DefaultSerializer.serializeErgoTree(tree1)
    val bytes2 = DefaultSerializer.serializeErgoTree(tree2)
    val (_, _, _, treeBytes1) = DefaultSerializer
      .deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes1))
    val (_, _, _, treeBytes2) = DefaultSerializer
      .deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes2))
    treeBytes1 shouldEqual treeBytes2
  }

  property("(de)serialize round trip") {
    // increased minimum number of successes
    // for better coverage of all possible combinations (with/without constants, segregation option, etc.)
    forAll(ergoTreeGen, minSuccessful(500)) { tree: ErgoTree =>
      val bytes = DefaultSerializer.serializeErgoTree(tree)
      val deserializedTree = DefaultSerializer.deserializeErgoTree(bytes)
      deserializedTree shouldEqual tree
    }
  }

  property("max ergo tree byte size check") {
    val tree = EQ(Plus(10, 20), IntConstant(30)).toSigmaProp.treeWithSegregation
    val r = SigmaSerializer.startReader(DefaultSerializer.serializeErgoTree(tree))
    an[InputSizeLimitExceeded] should be thrownBy DefaultSerializer.deserializeErgoTree(r, 1)
  }

  property("restore reader's positionLimit") {
    val tree = EQ(Plus(10, 20), IntConstant(30)).toSigmaProp.treeWithSegregation
    val r = SigmaSerializer.startReader(DefaultSerializer.serializeErgoTree(tree))
    r.positionLimit = 1
    DefaultSerializer.deserializeErgoTree(r) shouldEqual tree
    r.positionLimit shouldBe 1
  }
}
