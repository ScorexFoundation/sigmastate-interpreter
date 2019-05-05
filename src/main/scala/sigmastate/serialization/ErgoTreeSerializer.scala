package sigmastate.serialization

import org.ergoplatform.ValidationRules.CheckDeserializedScriptIsSigmaProp
import org.ergoplatform.{SoftForkException, ValidationRules, ValidationRule, ValidationSettings}
import sigmastate.{SType, TrivialProp}
import sigmastate.Values.{Value, ErgoTree, Constant}
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.lang.Terms.ValueOps
import sigmastate.lang.exceptions.SerializerException
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigma.util.Extensions._

import scala.language.reflectiveCalls
import scala.collection.mutable

class ErgoTreeSerializer {

  private def serializeWithoutSize(ergoTree: ErgoTree): Array[Byte] = {
    val w = SigmaSerializer.startWriter()
    serializeHeader(ergoTree, w)
    ValueSerializer.serialize(ergoTree.root, w)
    w.toBytes
  }

  /** Default serialization of ErgoTree. Doesn't apply any transformations and guarantee to preserve original
    * structure after deserialization. */
  def serializeErgoTree(ergoTree: ErgoTree): Array[Byte] = {
    val bytes = serializeWithoutSize(ergoTree)
    if (ergoTree.hasSize) {
      val w = SigmaSerializer.startWriter()
      val header = bytes(0)
      val contentLength = bytes.length - 1
      val contentBytes = new Array[Byte](contentLength)
      Array.copy(bytes, 1, contentBytes, 0, contentLength)  // TODO optimize: avoid new array by implementing putSlice(arr, from, len)
      w.put(header)
      w.putUInt(contentLength)
      w.putBytes(contentBytes)
      w.toBytes
    }
    else bytes
  }

  /** Default deserialization of ErgoTree (should be inverse to `serializeErgoTree`).
    * Doesn't apply any transformations to the parsed tree. */
  def deserializeErgoTree(bytes: Array[Byte])(implicit vs: ValidationSettings): ErgoTree  = {
    val r = SigmaSerializer.startReader(bytes)
    deserializeErgoTree(r)
  }

  def deserializeErgoTree(r: SigmaByteReader): ErgoTree  = {
    val (h, sizeOpt) = deserializeHeaderAndSize(r)
    val startPos = r.position
    val tree = try {
      val cs = deserializeConstants(h, r)
      val previousConstantStore = r.constantStore
      r.constantStore = new ConstantStore(cs)
      // reader with constant store attached is required (to get tpe for a constant placeholder)
      val root = ValueSerializer.deserialize(r)
      CheckDeserializedScriptIsSigmaProp(r.validationSettings, root) {}
      r.constantStore = previousConstantStore
      ErgoTree(h, cs, root.asSigmaProp)
    }
    catch {
      case sfe: SoftForkException =>
        sizeOpt match {
          case Some(treeSize) =>
            val endPos = startPos + treeSize
            r.position = endPos
            ErgoTree.fromSigmaBoolean(TrivialProp.TrueProp)
          case None =>
            throw sfe
        }
    }
    tree
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

  /** Deserialize `header` and optional `size` slots only. */
  private def deserializeHeaderAndSize(r: SigmaByteReader): (Byte, Option[Int]) = {
    val header = r.getByte()
    val sizeOpt = if (ErgoTree.hasSize(header)) {
      val constantSerializer = ConstantSerializer(DeserializationSigmaBuilder)
      val size = r.getUInt().toIntExact
      Some(size)
    } else
      None
    (header, sizeOpt)
  }

  /** Deserialize constants section only. */
  private def deserializeConstants(header: Byte, r: SigmaByteReader): Array[Constant[SType]] = {
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
    constants
  }

  /** Deserialize header and constant sections, but output the rest of the bytes as separate array. */
  def deserializeHeaderWithTreeBytes(r: SigmaByteReader): (Byte, Option[Int], Array[Constant[SType]], Array[Byte]) = {
    val (header, sizeOpt) = deserializeHeaderAndSize(r)
    val constants = deserializeConstants(header, r)
    val treeBytes = r.getBytes(r.remaining)
    (header, sizeOpt, constants, treeBytes)
  }

  def substituteConstants(scriptBytes: Array[Byte],
                          positions: Array[Int],
                          newVals: Array[Value[SType]])(implicit vs: ValidationSettings): Array[Byte] = {
    require(positions.length == newVals.length,
      s"expected positions and newVals to have the same length, got: positions: ${positions.toSeq},\n newVals: ${newVals.toSeq}")
    val r = SigmaSerializer.startReader(scriptBytes)
    val (header, _, constants, treeBytes) = deserializeHeaderWithTreeBytes(r)
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
