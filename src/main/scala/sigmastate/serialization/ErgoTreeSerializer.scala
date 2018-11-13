package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values.{Value, Constant, ErgoTree}
import sigmastate.lang.DeserializationSigmaBuilder
import scala.collection.mutable

object ErgoTreeSerializer {

  def serialize(ergoTree: ErgoTree): Array[Byte] = {
    val w = Serializer.startWriter()
    val constantSerializer = ConstantSerializer(DeserializationSigmaBuilder)
    w.put(ergoTree.header)
    w.putUInt(ergoTree.constants.length)
    ergoTree.constants.foreach(c => constantSerializer.serialize(c, w))
    ValueSerializer.serialize(ergoTree.root, w)
    w.toBytes
  }

  /** Serialize ErgoTree with DefaultHeader.
    * This method uses single traverse of the tree to both replace constants with ConstantPlaceholders
    * and also write the `tree` to the Writer's buffer obtaining `treeBytes`.
    * After the constants are collected the final byte array is composed by serializing constants and
    * then appending `treeBytes` */
  def serialize(tree: Value[SType]): Array[Byte] = {
    val constantStore = new ConstantStore()
    val treeWriter = Serializer.startWriter(constantStore)
    ValueSerializer.serialize(tree, treeWriter)
    val extractedConstants = constantStore.getAll
    val w = Serializer.startWriter()
    w.put(ErgoTree.DefaultHeader)

    // write constants
    w.putUInt(extractedConstants.length)
    val constantSerializer = ConstantSerializer(DeserializationSigmaBuilder)
    extractedConstants.foreach(c => constantSerializer.serialize(c, w))

    // write tree with constants replaced by placeholders during serialization
    w.putBytes(treeWriter.toBytes)
    w.toBytes
  }

  def treeWithPlaceholdersBytes(bytes: Array[Byte]): (Byte, IndexedSeq[Constant[SType]], Array[Byte]) = {
    val constantSerializer = ConstantSerializer(DeserializationSigmaBuilder)
    val r = Serializer.startReader(bytes)
    val header = r.getByte() // skip the header
    val constantCount = r.getUInt().toInt
    val constantsBuilder = mutable.ArrayBuilder.make[Constant[SType]]()
    for (_ <- 0 until constantCount) {
      constantsBuilder += constantSerializer.deserialize(r)
    }
    val constants = constantsBuilder.result
    val treeBytes = r.getBytes(r.remaining)
    (header, constants, treeBytes)
  }

  def deserialize(bytes: Array[Byte], resolvePlaceholdersToConstants: Boolean = true): Value[SType] = {
    // TODO optimize allocation/copying
    val (header, constants, treeBytes) = treeWithPlaceholdersBytes(bytes)
    val r = Serializer.startReader(treeBytes, new ConstantStore(constants),
      resolvePlaceholdersToConstants)
    val tree = ValueSerializer.deserialize(r)
    tree
  }

  def deserializeWithConstantInjection(constantStore: ConstantStore, treeBytes: Array[Byte]): Value[SType] = {
    val r = Serializer.startReader(treeBytes, constantStore, resolvePlaceholdersToConstants = true)
    val tree = ValueSerializer.deserialize(r)
    tree
  }
}

