package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values.{Constant, ErgoTree, Value}
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.lang.Terms.ValueOps
import sigmastate.lang.exceptions.SerializerException
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

import scala.collection.mutable

class ErgoTreeSerializer {

  /** Default serialization of ErgoTree. Doesn't apply any transformations and guarantee to preserve original
    * structure after deserialization. */
  def serializeErgoTree(ergoTree: ErgoTree): Array[Byte] = {
    val w = SigmaSerializer.startWriter()
    serializeHeader(ergoTree, w)
    w.putValue(ergoTree.root)
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

  /** Deserialize header and constant sections, but output the rest of the bytes as separate array. */
  def deserializeHeaderWithTreeBytes(r: SigmaByteReader): (Byte, Array[Constant[SType]], Array[Byte]) = {
    val (header, constants) = deserializeHeader(r)
    val treeBytes = r.getBytes(r.remaining)
    (header, constants, treeBytes)
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
        valW.putValue(newVal)
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
