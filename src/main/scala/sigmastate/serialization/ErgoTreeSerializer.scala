package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values.{Constant, Value}
import sigmastate.lang.DeserializationSigmaBuilder

import scala.collection.mutable

object ErgoTreeSerializer {

  def serialize(constants: IndexedSeq[Constant[SType]], tree: Value[SType]): Array[Byte] = {
    val w = Serializer.startWriter()
    val constantSerializer = ConstantSerializer(DeserializationSigmaBuilder)
    w.put(0) // header: reserved for future flags
    w.putUInt(constants.length)
    constants.foreach(c => constantSerializer.serialize(c, w))
    ValueSerializer.serialize(tree, w)
    w.toBytes
  }

  def treeWithPlaceholdersBytes(bytes: Array[Byte]): (IndexedSeq[Constant[SType]], Array[Byte]) = {
    val constantSerializer = ConstantSerializer(DeserializationSigmaBuilder)
    val r = Serializer.startReader(bytes)
    r.getByte() // skip the header
    val constantCount = r.getUInt().toInt
    val constantsBuilder = mutable.ArrayBuilder.make[Constant[SType]]()
    for (_ <- 0 until constantCount) {
      constantsBuilder += constantSerializer.deserialize(r)
    }
    val constants = constantsBuilder.result
    val treeBytes = r.getBytes(r.remaining)
    (constants, treeBytes)
  }

  def deserialize(bytes: Array[Byte]): Value[SType] = {
    val (constants, treeBytesArray) = treeWithPlaceholdersBytes(bytes)
    val r = Serializer.startReader(treeBytesArray)
    r.payload = new ConstantStore(constants)
    val tree = ValueSerializer.deserialize(r)
    tree
  }

  def deserializeWithConstantInjection(constantStore: ConstantStore, treeBytes: Array[Byte]): Value[SType] = {
    val r = Serializer.startReader(treeBytes)
    r.payload = constantStore
    val tree = ValueSerializer.deserialize(r)
    tree
  }
}

