package sigmastate.serialization

import sigmastate.SCollection.SByteArray
import sigmastate.Values.{ByteArrayConstant, ConcreteCollection, Constant, ErgoTree, Value}
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.utils.SigmaByteReader
import sigmastate.utxo.Append
import sigmastate.{SGroupElement, SType}

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
    deserialize(Serializer.startReader(bytes), resolvePlaceholdersToConstants)
  }

  def deserialize(r: SigmaByteReader, resolvePlaceholdersToConstants: Boolean): Value[SType] = {
    val constantSerializer = ConstantSerializer(DeserializationSigmaBuilder)
    r.getByte() // skip the header
    val constantCount = r.getUInt().toInt
    val constantsBuilder = mutable.ArrayBuilder.make[Constant[SType]]()
    for (_ <- 0 until constantCount) {
      constantsBuilder += constantSerializer.deserialize(r)
    }
    val constants = constantsBuilder.result
    val previousConstantStore = r.constantStore
    r.constantStore = new ConstantStore(constants)
    val previousResolvePlaceholderValue = r.resolvePlaceholdersToConstants
    r.resolvePlaceholdersToConstants = resolvePlaceholdersToConstants
    val tree = ValueSerializer.deserialize(r)
    r.constantStore = previousConstantStore
    r.resolvePlaceholdersToConstants = previousResolvePlaceholderValue
    tree
  }

  def deserializeWithConstantInjection(constantStore: ConstantStore, treeBytes: Array[Byte]): Value[SType] = {
    val r = Serializer.startReader(treeBytes, constantStore, resolvePlaceholdersToConstants = true)
    val tree = ValueSerializer.deserialize(r)
    tree
  }

  def serializedPubkeyPropValue(pubkey: Value[SByteArray]): Value[SByteArray] =
    Append(
      Append(
        ConcreteCollection(
          0.toByte, // header
          1.toByte, // const count
          SGroupElement.typeCode // const type
        ),
        pubkey // const value
      ),
      ConcreteCollection(
        OpCodes.ProveDlogCode,
        OpCodes.ConstantPlaceholderIndexCode,
        0.toByte // constant index in the store
      )
    )

  def substituteConstants(scriptBytes: Array[Byte],
                          positions: Array[Int],
                          newVals: Array[Value[SType]]): Array[Byte] = {
    require(positions.length == newVals.length,
      s"expected positions and newVals to have the same length, got: positions: ${positions.toSeq},\n newVals: ${newVals.toSeq}")
    val (header, constants, treeBytes) = treeWithPlaceholdersBytes(scriptBytes)

    val w = Serializer.startWriter()
    w.put(header)
    w.putUInt(constants.length)
    val constantSerializer = ConstantSerializer(DeserializationSigmaBuilder)

    constants.zipWithIndex.foreach {
      case (_, i) if positions.contains(i) =>
        val newVal = newVals(positions.indexOf(i))
        // we need to get newVal's serialized constant value (see ProveDlogSerializer for example)
        val constantStore = new ConstantStore()
        val valW = Serializer.startWriter(constantStore)
        ValueSerializer.serialize(newVal, valW)
        val newConsts = constantStore.getAll
        assert(newConsts.length == 1)
        constantSerializer.serialize(newConsts.head, w)
      case (c, _) =>
        constantSerializer.serialize(c, w)
    }

    w.putBytes(treeBytes)
    w.toBytes
  }
}

