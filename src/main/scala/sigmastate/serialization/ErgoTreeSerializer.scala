package sigmastate.serialization

import java.nio.ByteBuffer

import sigmastate.SType
import sigmastate.Values.{Constant, Value}
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.utils.{ByteArrayBuilder, ByteArrayWriter, SigmaByteReader, SigmaByteWriter}

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

  def serialize(tree: Value[SType]): Array[Byte] = {
    val constantStore = new ConstantStore()
    val treeWriter = Serializer.startWriter(constantStore)
    ValueSerializer.serialize(tree, treeWriter)
    val extractedConstants = constantStore.getAll
    val w = Serializer.startWriter()
    // header: reserved for future flags
    w.put(0)
    // write constants
    w.putUInt(extractedConstants.length)
    val constantSerializer = ConstantSerializer(DeserializationSigmaBuilder)
    extractedConstants.foreach(c => constantSerializer.serialize(c, w))
    // write tree with constant placeholders
    w.putBytes(treeWriter.toBytes)
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
    // TODO optimize allocation/copying
    val (constants, treeBytesArray) = treeWithPlaceholdersBytes(bytes)
    val r = Serializer.startReader(treeBytesArray, new ConstantStore(constants))
    val tree = ValueSerializer.deserialize(r)
    tree
  }

  def deserializeWithConstantInjection(constantStore: ConstantStore, treeBytes: Array[Byte]): Value[SType] = {
    val r = Serializer.startReader(treeBytes, constantStore)
    val tree = ValueSerializer.deserialize(r)
    tree
  }
}

