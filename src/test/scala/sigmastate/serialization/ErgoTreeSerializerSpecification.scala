package sigmastate.serialization

import org.ergoplatform.Self
import sigmastate.Values.ErgoTree.DefaultHeader
import sigmastate.Values.{ErgoTree, IntConstant, LongConstant, SigmaPropValue}
import sigmastate._
import sigmastate.eval.IRContext
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.lang.exceptions.SerializerException
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.utxo.ExtractAmount

class ErgoTreeSerializerSpecification extends SerializationSpecification with SigmaTestingCommons {

  implicit lazy val IR: TestingIRContext = new TestingIRContext {
    beginPass(noConstPropagationPass)
  }

  private def passThroughTreeBuilder(tree: SigmaPropValue): SigmaPropValue = {
    val env = Map[String, Any]()
    val IR.Pair(calcF, _) = IR.doCosting(env, tree)
    val outTree = IR.buildTree(calcF, None)
    outTree
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
    val (_, deserializedConstants, treeBytes) = DefaultSerializer
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
    val prop = IntConstant(1)
    val bytes = DefaultSerializer.serializeErgoTree(extractConstants(prop.asInstanceOf[SigmaPropValue]))
    an[SerializerException] should be thrownBy DefaultSerializer.deserializeErgoTree(bytes)
    an[SerializerException] should be thrownBy DefaultSerializer.deserializeErgoTree(bytes)
  }

  property("Constant extraction during serialization: (de)serialization round trip") {
    val tree = EQ(Plus(10, 20), IntConstant(30)).toSigmaProp.treeWithSegregation
    val bytes = DefaultSerializer.serializeErgoTree(tree)
    val (_, deserializedConstants, _) = DefaultSerializer.
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
    val (_, _, treeBytes1) = DefaultSerializer
      .deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes1))
    val (_, _, treeBytes2) = DefaultSerializer
      .deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes2))
    treeBytes1 shouldEqual treeBytes2
  }

  property("(de)serialize round trip (without constants)") {
    val prop = EQ(ExtractAmount(Self), LongConstant(0)).toSigmaProp
    val tree = ErgoTree(DefaultHeader, IndexedSeq(), prop, prop)
    val bytes = DefaultSerializer.serializeErgoTree(tree)
    val deserializedProp = DefaultSerializer.deserializeErgoTree(bytes).proposition
    deserializedProp shouldEqual prop
  }

  property("AND expr gen: (de)serializer round trip") {
    forAll(logicalExprTreeNodeGen(Seq(AND.apply))) { tree =>
      val processedTree = passThroughTreeBuilder(tree.toSigmaProp)
      val ergoTree = extractConstants(processedTree)
      val bytes = DefaultSerializer.serializeErgoTree(ergoTree)
      val deserializedTree = DefaultSerializer.deserializeErgoTree(bytes)
      deserializedTree shouldEqual ergoTree
    }
  }

}
