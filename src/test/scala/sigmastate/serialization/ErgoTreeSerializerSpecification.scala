package sigmastate.serialization

import java.nio.ByteBuffer

import org.ergoplatform.Self
import sigmastate.Values.{BlockValue, Constant, ConstantPlaceholder, IntConstant, ValDef, ValUse, Value}
import sigmastate._
import sigmastate.eval.IRContext
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.utils.SigmaByteReader
import sigmastate.utxo.ExtractAmount

class ErgoTreeSerializerSpecification extends SerializationSpecification with SigmaTestingCommons {

  implicit lazy val IR: TestingIRContext = new TestingIRContext

  private def passThroughTreeBuilder(tree: Value[SType]): Value[SType] = {
    val env = Map[String, Any]()
    val IR.Pair(calcF, _) = IR.doCosting(env, tree)
    val outTree = IR.buildTree(calcF, None)
    outTree
  }

  private def extractConstants(tree: Value[SType])(implicit IR: IRContext): (IndexedSeq[Constant[SType]], Value[SType]) = {
    val env = Map[String, Any]()
    val IR.Pair(calcF, _) = IR.doCosting(env, tree)
    val extractConstants = new ConstantStore()
    val outTree = IR.buildTree(calcF, Some(extractConstants))
    (extractConstants.getAll, outTree)
  }

  property("(de)serialization round trip using treeBytes()") {
    val tree = Plus(10, 20)
    val (constants, treeWithPlaceholders) = extractConstants(tree)
    val bytes = ErgoTreeSerializer.serialize(constants, treeWithPlaceholders)
    val (deserializedConstants, treeBytes) = ErgoTreeSerializer.treeWithPlaceholdersBytes(bytes)
    deserializedConstants shouldEqual constants
    val r = Serializer.startReader(treeBytes, new ConstantStore(deserializedConstants))
    val deserializedTree = ValueSerializer.deserialize(r)
    deserializedTree shouldEqual tree
  }

  property("Constant extraction via compiler pass: (de)serialization round trip") {
    val tree = Plus(10, 20)
    val (constants, treeWithPlaceholders) = extractConstants(tree)
    val bytes = ErgoTreeSerializer.serialize(constants, treeWithPlaceholders)
    val deserializedTree = ErgoTreeSerializer.deserialize(bytes)
    deserializedTree shouldEqual tree
  }

  property("Constant extraction during serialization: (de)serialization round trip") {
    val tree = Plus(10, 20)
    val bytes = ErgoTreeSerializer.serialize(tree)
    val (deserializedConstants, _) = ErgoTreeSerializer.treeWithPlaceholdersBytes(bytes)
    deserializedConstants.size shouldBe 2
    val deserializedTree = ErgoTreeSerializer.deserialize(bytes)
    deserializedTree shouldEqual tree
  }

  property("tree with placeholders bytes should be equal if only constants are different") {
    val tree1 = Plus(10, 20)
    val tree2 = Plus(30, 40)
    val bytes1 = ErgoTreeSerializer.serialize(tree1)
    val bytes2 = ErgoTreeSerializer.serialize(tree2)
    val (_, treeBytes1) = ErgoTreeSerializer.treeWithPlaceholdersBytes(bytes1)
    val (_, treeBytes2) = ErgoTreeSerializer.treeWithPlaceholdersBytes(bytes2)
    treeBytes1 shouldEqual treeBytes2
  }

  property("(de)serialize round trip (without constants)") {
    val tree = ExtractAmount(Self)
    val bytes = ErgoTreeSerializer.serialize(IndexedSeq(), tree)
    val deserializedTree = ErgoTreeSerializer.deserialize(bytes)
    deserializedTree shouldEqual tree
  }

  property("AND expr gen: (de)serializer round trip") {
    forAll(logicalExprTreeNodeGen(Seq(AND.apply))) { tree =>
      val processedTree = passThroughTreeBuilder(tree)
      val (constants, treeWithPlaceholders) = extractConstants(processedTree)
      val bytes = ErgoTreeSerializer.serialize(constants, treeWithPlaceholders)
      val deserializedTree = ErgoTreeSerializer.deserialize(bytes)
      deserializedTree shouldEqual processedTree
    }
  }

  property("AND expr gen: deserialization round trip with constant injection") {
    forAll(logicalExprTreeNodeGen(Seq(AND.apply))) { tree =>
      val processedTree = passThroughTreeBuilder(tree)
      val (constants, treeWithPlaceholders) = extractConstants(processedTree)
      val bytes = ErgoTreeSerializer.serialize(constants, treeWithPlaceholders)
      val (deserializedConstants, treeBytes) = ErgoTreeSerializer.treeWithPlaceholdersBytes(bytes)
      val c = new ConstantStore(deserializedConstants)
      val deserializedTree = ErgoTreeSerializer.deserializeWithConstantInjection(c, treeBytes)
      deserializedTree shouldEqual processedTree
    }
  }

}
