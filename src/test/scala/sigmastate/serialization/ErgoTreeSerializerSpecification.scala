package sigmastate.serialization

import org.ergoplatform.Self
import sigmastate.Values.{BlockValue, Constant, ConstantPlaceholder, IntConstant, ValDef, ValUse, Value}
import sigmastate._
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.utxo.ExtractAmount

class ErgoTreeSerializerSpecification extends SerializationSpecification with SigmaTestingCommons {

  implicit lazy val IR: TestingIRContext = new TestingIRContext

  private def passThroughTreeBuilder(tree: Value[SType]): Value[SType] = {
    val env = Map[String, Any]()
    val IR.Pair(calcF, _) = IR.doCosting(env, tree)
    val outTree = IR.buildTree(calcF, None)
    outTree
  }

  property("(de)serialization round trip using treeBytes()") {
    val tree = Plus(10, 20)
    val (constants, treeWithPlaceholders) = ConstantStore.extractConstants(tree)
    val bytes = ErgoTreeSerializer.serialize(constants, treeWithPlaceholders)
    val (deserializedConstants, treeBytes) = ErgoTreeSerializer.treeWithPlaceholdersBytes(bytes)
    val deserializedTreeWithPlaceholders = ValueSerializer.deserialize(treeBytes)
    deserializedTreeWithPlaceholders shouldEqual treeWithPlaceholders
    deserializedConstants shouldEqual constants
  }

  property("(de)serialization round trip using deserialize()") {
    val tree = Plus(10, 20)
    val (constants, treeWithPlaceholders) = ConstantStore.extractConstants(tree)
    val bytes = ErgoTreeSerializer.serialize(constants, treeWithPlaceholders)
    val (deserializedConstants, deserializedTreeWithPlaceholders) = ErgoTreeSerializer.deserialize(bytes)
    deserializedTreeWithPlaceholders shouldEqual treeWithPlaceholders
    deserializedConstants shouldEqual constants
  }

  property("extract constants") {
    val script = Plus(10, 20)
    val extractedConstants = Seq(IntConstant(10), IntConstant(20))
    val scriptWithPlaceholders = Plus(ConstantPlaceholder(0, SInt), ConstantPlaceholder(1, SInt))
    ConstantStore.extractConstants(script) shouldEqual (extractedConstants, scriptWithPlaceholders)
  }

  property("no constants to extract") {
    val script = Plus(ExtractAmount(Self), ExtractAmount(Self))
    val extractedConstants = Seq()
    val scriptWithPlaceholders = BlockValue(Vector(ValDef(1,List(),ExtractAmount(Self))),Plus(ValUse(1,SLong),ValUse(1,SLong)))
    ConstantStore.extractConstants(script) shouldEqual (extractedConstants, scriptWithPlaceholders)
  }

  property("(de)serialize round trip (without constants)") {
    val script = ExtractAmount(Self)
    val bytes = ErgoTreeSerializer.serialize(IndexedSeq(), script)
    val (deserializedConstants, deserializedTreeWithPlaceholders) = ErgoTreeSerializer.deserialize(bytes)
    deserializedTreeWithPlaceholders shouldEqual script
    deserializedConstants shouldBe empty
  }

  property("AND expr gen: (de)serializer round trip") {
    forAll(logicalExprTreeNodeGen(Seq(AND.apply))) { tree =>
      val processedTree = passThroughTreeBuilder(tree)
      val (constants, treeWithPlaceholders) = ConstantStore.extractConstants(processedTree)
      val bytes = ErgoTreeSerializer.serialize(constants, treeWithPlaceholders)
      val (deserializedConstants, deserializedTree) = ErgoTreeSerializer.deserialize(bytes)
      deserializedConstants shouldEqual constants
      deserializedTree shouldEqual treeWithPlaceholders
    }
  }

  property("AND expr gen: deserialization round trip with constant injection") {
    forAll(logicalExprTreeNodeGen(Seq(AND.apply))) { tree =>
      val processedTree = passThroughTreeBuilder(tree)
      val (constants, treeWithPlaceholders) = ConstantStore.extractConstants(processedTree)
      val bytes = ErgoTreeSerializer.serialize(constants, treeWithPlaceholders)
      val (deserializedConstants, treeBytes) = ErgoTreeSerializer.treeWithPlaceholdersBytes(bytes)
      val c = new ConstantStore(deserializedConstants)
      val deserializedTree = ErgoTreeSerializer.deserializeWithConstantInjection(c, treeBytes)
      deserializedTree shouldEqual processedTree
    }
  }

}
