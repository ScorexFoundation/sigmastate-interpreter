package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values.{Constant, Value}
import sigmastate.eval.IRContext
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.utils.Extensions._

import scala.collection.mutable

class ErgoTreeSerializer(IR: IRContext) {
  import IR._

  def serialize(tree: Value[SType]): Array[Byte] = {
    val (extractedConstants, treeWithPlaceholders) = extractConstants(tree)
    val w = Serializer.startWriter()
    val constantSerializer = ConstantSerializer(DeserializationSigmaBuilder)
    w.putUInt(extractedConstants.length)
    extractedConstants.foreach(c => constantSerializer.serialize(c, w))
    ValueSerializer.serialize(treeWithPlaceholders, w)
    w.toBytes
  }

  def deserializeRaw(bytes: Array[Byte]): (IndexedSeq[Constant[SType]], Value[SType]) = {
    val constantSerializer = ConstantSerializer(DeserializationSigmaBuilder)
    val r = Serializer.startReader(bytes)
    val constantCount = r.getUInt().toInt
    val constantsBuilder = mutable.ArrayBuilder.make[Constant[SType]]()
    for (_ <- 0 until constantCount) {
      constantsBuilder += constantSerializer.deserialize(r)
    }
    val constants = constantsBuilder.result
    val tree = r.getValue()
    (constants, tree)
  }

  def deserialize(bytes: Array[Byte]): Value[SType] = {
    val (constants, tree) = deserializeRaw(bytes)
    if (constants.nonEmpty)
      injectConstants(constants, tree)
    else
      tree
  }

  def extractConstants(tree: Value[SType]): (IndexedSeq[Constant[SType]], Value[SType]) = {
    val env = Map[String, Any]()
    val Pair(calcF, _) = doCosting(env, tree)
    val extractConstants = new ExtractConstants()
    val outTree = IR.buildTree(calcF, Some(extractConstants))
    (extractConstants.extractedConstants(), outTree)
  }

  def injectConstants(constants: IndexedSeq[Constant[SType]], tree: Value[SType]): Value[SType] = {
    val env = Map[String, Any]()
    val Pair(calcF, _) = doCosting(env, tree)
    val outTree = IR.buildTree(calcF, Some(new InjectConstants(constants)))
    outTree
  }
}

object ErgoTreeSerializer {
  def apply(IR: IRContext): ErgoTreeSerializer = new ErgoTreeSerializer(IR)
}
