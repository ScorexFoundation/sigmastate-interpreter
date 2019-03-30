package sigmastate.serialization

import sigmastate.SCollection.SByteArray
import sigmastate.Values.{ConcreteCollection, Constant, ErgoTree, Value}
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.Append
import sigmastate.{SGroupElement, SType}
import sigmastate.lang.Terms.ValueOps
import sigmastate.lang.exceptions.{SerializerException, SigmaException}

import scala.collection.mutable

class ErgoTreeSerializer {

  /** Default serialization of ErgoTree. Doesn't apply any transformations and guarantee to preserve original
    * structure after deserialization. */
  def serializeErgoTree(ergoTree: ErgoTree): Array[Byte] = {
    val w = SigmaSerializer.startWriter()
    serializeHeader(ergoTree, w)
    ValueSerializer.serialize(ergoTree.root, w)
    w.toBytes
  }

  /** Default deserialization of ErgoTree (should be inverse to `serializeErgoTree`).
    * Doesn't apply any transformations to the parsed tree. */
  def deserializeErgoTree(bytes: Array[Byte]): ErgoTree  = {
    val r = SigmaSerializer.startReader(bytes)
    deserializeErgoTree(r)
  }

  def deserializeErgoTree(r: SigmaByteReader): ErgoTree  = {
    val (h, cs) = deserializeHeader(r)
    val previousConstantStore = r.constantStore
    r.constantStore = new ConstantStore(cs)
    // reader with constant store attached is required (to get tpe for a constant placeholder)
    val root = ValueSerializer.deserialize(r)
    if (!root.tpe.isSigmaProp)
      throw new SerializerException(s"Failed deserialization, expected deserialized script to have type SigmaProp; got ${root.tpe}")
    r.constantStore = previousConstantStore
    ErgoTree(h, cs, root.asSigmaProp)
  }

  /** Serialize header and constants section only.*/
  private def serializeHeader(ergoTree: ErgoTree, w: SigmaByteWriter): Unit = {
    w.put(ergoTree.header)
    if (ergoTree.isConstantSegregation) {
      val constantSerializer = ConstantSerializer(DeserializationSigmaBuilder)
      w.putUInt(ergoTree.constants.length)
      ergoTree.constants.foreach(c => constantSerializer.serialize(c, w))
    }
  }

  /** Deserialize header and constants section only. */
  private def deserializeHeader(r: SigmaByteReader): (Byte, Array[Constant[SType]]) = {
    val header = r.getByte()
    val constants = if (ErgoTree.isConstantSegregation(header)) {
      val constantSerializer = ConstantSerializer(DeserializationSigmaBuilder)
      val nConsts = r.getUInt().toInt
      val builder = mutable.ArrayBuilder.make[Constant[SType]]()
      for (_ <- 0 until nConsts) {
        builder += constantSerializer.deserialize(r)
      }
      builder.result
    }
    else
      Array.empty[Constant[SType]]
    (header, constants)
  }

  /** Serialize Value with ConstantSegregationHeader, constants segregated from the tree and ConstantPlaceholders
    * referring to the segregated constants.
    *
    * This method uses single traverse of the tree to:
    * 1) find and segregate all constants;
    * 2) replace constants with ConstantPlaceholders in the `value`;
    * 3) write the `value` to the Writer's buffer obtaining `valueBytes`.
    *
    * After the constants are collected the final byte array is composed by serializing constants and
    * then appending `valueBytes` */
  def serializeWithSegregation(value: Value[SType]): Array[Byte] = {
    val constantStore = new ConstantStore()
    val byteWriter = SigmaSerializer.startWriter(constantStore)

    // serialize value and segregate constants into constantStore
    ValueSerializer.serialize(value, byteWriter)
    val extractedConstants = constantStore.getAll

    val w = SigmaSerializer.startWriter()
    serializeHeader(ErgoTree(ErgoTree.ConstantSegregationHeader, extractedConstants, null), w)

    // write tree bytes with ConstantsPlaceholders (which were injected during serialization)
    w.putBytes(byteWriter.toBytes)
    w.toBytes
  }

  /** Deserialize header and constant sections, but output the rest of the bytes as separate array. */
  def deserializeHeaderWithTreeBytes(r: SigmaByteReader): (Byte, Array[Constant[SType]], Array[Byte]) = {
    val (header, constants) = deserializeHeader(r)
    val treeBytes = r.getBytes(r.remaining)
    (header, constants, treeBytes)
  }

  def deserialize(bytes: Array[Byte], resolvePlaceholdersToConstants: Boolean = true): Value[SType] = {
    deserialize(SigmaSerializer.startReader(bytes), resolvePlaceholdersToConstants)
  }

  /** Deserialize Value replacing placeholders with constants if the parameter is true. */
  def deserialize(r: SigmaByteReader, resolvePlaceholdersToConstants: Boolean): Value[SType] = {
    val (header, constants) = deserializeHeader(r)
    require(!resolvePlaceholdersToConstants || ErgoTree.isConstantSegregation(header),
      s"Invalid arguments of ErgoTreeSerializer.deserialize: resolvePlaceholdersToConstants=$resolvePlaceholdersToConstants, header=$header")

    val previousConstantStore = r.constantStore
    r.constantStore = new ConstantStore(constants)
    val previousResolvePlaceholderValue = r.resolvePlaceholdersToConstants
    r.resolvePlaceholdersToConstants = resolvePlaceholdersToConstants
    val value = ValueSerializer.deserialize(r)
    r.constantStore = previousConstantStore
    r.resolvePlaceholdersToConstants = previousResolvePlaceholderValue
    value
  }

  def deserializeWithConstantInjection(constantStore: ConstantStore, treeBytes: Array[Byte]): Value[SType] = {
    val r = SigmaSerializer.startReader(treeBytes, constantStore, resolvePlaceholdersToConstants = true)
    val tree = ValueSerializer.deserialize(r)
    tree
  }

  def substituteConstants(scriptBytes: Array[Byte],
                          positions: Array[Int],
                          newVals: Array[Value[SType]]): Array[Byte] = {
    require(positions.length == newVals.length,
      s"expected positions and newVals to have the same length, got: positions: ${positions.toSeq},\n newVals: ${newVals.toSeq}")
    val r = SigmaSerializer.startReader(scriptBytes)
    val (header, constants, treeBytes) = deserializeHeaderWithTreeBytes(r)
    val w = SigmaSerializer.startWriter()
    w.put(header)
    w.putUInt(constants.length)
    val constantSerializer = ConstantSerializer(DeserializationSigmaBuilder)

    constants.zipWithIndex.foreach {
      case (c, i) if positions.contains(i) =>
        val newVal = newVals(positions.indexOf(i))
        // we need to get newVal's serialized constant value (see ProveDlogSerializer for example)
        val constantStore = new ConstantStore()
        val valW = SigmaSerializer.startWriter(constantStore)
        ValueSerializer.serialize(newVal, valW)
        val newConsts = constantStore.getAll
        assert(newConsts.length == 1)
        val newConst = newConsts.head
        assert(c.tpe == newConst.tpe, s"expected new constant to have the same ${c.tpe} tpe, got ${newConst.tpe}")
        constantSerializer.serialize(newConst, w)
      case (c, _) =>
        constantSerializer.serialize(c, w)
    }

    w.putBytes(treeBytes)
    w.toBytes
  }
}

object ErgoTreeSerializer {
  val DefaultSerializer = new ErgoTreeSerializer
}
