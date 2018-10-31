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
  property("extract constants") {
    val script = Plus(10, 20)
    val extractedConstants = Seq(IntConstant(10), IntConstant(20))
    val scriptWithPlaceholders = Plus(ConstantPlaceholder(0, SInt), ConstantPlaceholder(1, SInt))
    ErgoTreeSerializer(IR).extractConstants(script) shouldEqual (extractedConstants, scriptWithPlaceholders)
  }

  property("no constants to extract") {
    val script = Plus(ExtractAmount(Self), ExtractAmount(Self))
    val extractedConstants = Seq()
    val scriptWithPlaceholders = BlockValue(Vector(ValDef(1,List(),ExtractAmount(Self))),Plus(ValUse(1,SLong),ValUse(1,SLong)))
    ErgoTreeSerializer(IR).extractConstants(script) shouldEqual (extractedConstants, scriptWithPlaceholders)
  }

  property("inject constants") {
    val script = Plus(10, 20)
    val extractedConstants = Seq(IntConstant(10), IntConstant(20)).asInstanceOf[Seq[Constant[SType]]]
    val scriptWithPlaceholders = Plus(ConstantPlaceholder(0, SInt), ConstantPlaceholder(1, SInt))
    ErgoTreeSerializer(IR).injectConstants(extractedConstants, scriptWithPlaceholders) shouldEqual script
  }

  property("deserializeRaw") {
    val script = Plus(10, 20)
    val extractedConstants = Seq(IntConstant(10), IntConstant(20))
    val scriptWithPlaceholders = Plus(ConstantPlaceholder(0, SInt), ConstantPlaceholder(1, SInt))
    val bytes = ErgoTreeSerializer(IR).serialize(script)
    ErgoTreeSerializer(IR).deserializeRaw(bytes) shouldEqual (extractedConstants, scriptWithPlaceholders)
  }

  property("(de)serialize round trip (with constants)") {
    val script = Plus(10, 20)
    val bytes = ErgoTreeSerializer(IR).serialize(script)
    ErgoTreeSerializer(IR).deserialize(bytes) shouldEqual script
  }

  property("(de)serialize round trip (without constants)") {
    val script = ExtractAmount(Self)
    val bytes = ErgoTreeSerializer(IR).serialize(script)
    ErgoTreeSerializer(IR).deserialize(bytes) shouldEqual script
  }

  property("AND expr gen: extract/inject constants round trip") {
    forAll(logicalExprTreeNodeGen(Seq(AND.apply))) { tree =>
      val processedTree = passThroughTreeBuilder(tree)
//      println(s"processed tree: $processedTree")
      val (constants, treeWithPlaceholders) = ErgoTreeSerializer(IR).extractConstants(processedTree)
//      println(s"processed tree with placeholders: $treeWithPlaceholders")
      ErgoTreeSerializer(IR).injectConstants(constants, treeWithPlaceholders) shouldEqual processedTree
    }
  }

  property("AND expr gen: Serializer round trip") {
    forAll(logicalExprTreeNodeGen(Seq(AND.apply))) { tree =>
      val processedTree = passThroughTreeBuilder(tree)
      val bytes = ErgoTreeSerializer(IR).serialize(processedTree)
      ErgoTreeSerializer(IR).deserialize(bytes) shouldEqual processedTree
    }
  }
}
