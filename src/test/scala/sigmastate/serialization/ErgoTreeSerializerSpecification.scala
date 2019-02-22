package sigmastate.serialization

import java.nio.ByteBuffer

import org.ergoplatform.Self
import sigmastate.Values.ErgoTree.DefaultHeader
import sigmastate.Values.{BlockValue, Constant, ConstantPlaceholder, ErgoTree, IntConstant, LongConstant, ValDef, ValUse, Value}
import sigmastate._
import sigmastate.eval.IRContext
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.utils.SigmaByteReader
import sigmastate.utxo.ExtractAmount

class ErgoTreeSerializerSpecification extends SerializationSpecification with SigmaTestingCommons {

  implicit lazy val IR: TestingIRContext = new TestingIRContext {
    beginPass(noConstPropagationPass)
  }

  private def passThroughTreeBuilder(tree: Value[SType]): Value[SType] = {
    val env = Map[String, Any]()
    val IR.Pair(calcF, _) = IR.doCosting(env, tree)
    val outTree = IR.buildTree(calcF, None)
    outTree
  }

  private def extractConstants(tree: Value[SType])(implicit IR: IRContext): ErgoTree = {
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
    val tree = Plus(10, 20)
    val ergoTree = extractConstants(tree)
    val bytes = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(ergoTree)
    val (_, deserializedConstants, treeBytes) = ErgoTreeSerializer.DefaultSerializer
      .deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes))
    deserializedConstants shouldEqual ergoTree.constants
    val r = SigmaSerializer.startReader(treeBytes, new ConstantStore(deserializedConstants),
      resolvePlaceholdersToConstants = true)
    val deserializedTree = ValueSerializer.deserialize(r)
    deserializedTree shouldEqual tree
  }

  property("Constant extraction via compiler pass: (de)serialization round trip") {
    val prop = Plus(10, 20)
    val ergoTree = extractConstants(prop)
    val bytes = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(ergoTree)
    val deserializedTree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)
    deserializedTree shouldEqual ergoTree
  }

  property("Constant extraction during serialization: (de)serialization round trip") {
    val tree = Plus(10, 20)
    val bytes = ErgoTreeSerializer.DefaultSerializer.serializeWithSegregation(tree)
    val (_, deserializedConstants, _) = ErgoTreeSerializer.DefaultSerializer.
      deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes))
    deserializedConstants.length shouldBe 2
    val deserializedTree = ErgoTreeSerializer.DefaultSerializer.deserialize(bytes)
    deserializedTree shouldEqual tree
  }

  property("tree with placeholders bytes should be equal if only constants are different") {
    val tree1 = Plus(10, 20)
    val tree2 = Plus(30, 40)
    val bytes1 = ErgoTreeSerializer.DefaultSerializer.serializeWithSegregation(tree1)
    val bytes2 = ErgoTreeSerializer.DefaultSerializer.serializeWithSegregation(tree2)
    val (_, _, treeBytes1) = ErgoTreeSerializer.DefaultSerializer
      .deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes1))
    val (_, _, treeBytes2) = ErgoTreeSerializer.DefaultSerializer
      .deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes2))
    treeBytes1 shouldEqual treeBytes2
  }

  property("(de)serialize round trip (without constants)") {
    val prop = EQ(ExtractAmount(Self), LongConstant(0)).toSigmaProp
    val tree = ErgoTree(DefaultHeader, IndexedSeq(), prop, prop)
    val bytes = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(tree)
    val deserializedProp = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes).proposition
    deserializedProp shouldEqual prop
  }

  property("AND expr gen: (de)serializer round trip") {
    forAll(logicalExprTreeNodeGen(Seq(AND.apply))) { tree =>
      val processedTree = passThroughTreeBuilder(tree)
      val ergoTree = extractConstants(processedTree)
      val bytes = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(ergoTree)
      val deserializedTree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)
      deserializedTree shouldEqual ergoTree
    }
  }

  property("AND expr gen: deserialization round trip with constant injection") {
    forAll(logicalExprTreeNodeGen(Seq(AND.apply))) { tree =>
      val processedTree = passThroughTreeBuilder(tree)
      val ergoTree = extractConstants(processedTree)
      val bytes = ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(ergoTree)
      val (_, deserializedConstants, treeBytes) = ErgoTreeSerializer.DefaultSerializer
        .deserializeHeaderWithTreeBytes(SigmaSerializer.startReader(bytes))
      val c = new ConstantStore(deserializedConstants)
      val deserializedTree = ErgoTreeSerializer.DefaultSerializer.deserializeWithConstantInjection(c, treeBytes)
      deserializedTree shouldEqual processedTree
    }
  }

}
