package sigmastate.serialization


import org.ergoplatform.validation.ValidationRules.{CheckDeserializedScriptIsSigmaProp, CheckHeaderSizeBit}
import org.ergoplatform.validation.{ValidationException, SigmaValidationSettings}
import sigmastate.SType
import sigmastate.Values.{Value, ErgoTree, Constant, UnparsedErgoTree}
import sigmastate.lang.DeserializationSigmaBuilder
import sigmastate.lang.Terms.ValueOps
import sigmastate.lang.exceptions.SerializerException
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigma.util.Extensions._
import sigmastate.Values.ErgoTree.EmptyConstants

import scala.collection.mutable

/**
  * Rationale for soft-forkable ErgoTree serialization.
  * There are 2 points:
  *
  * 1) we can make size bit obligatory, i.e. always save total size of script body
  * (in this case we don't need size bit in the header). This will allow to
  * always skip right number of bytes in case of any exception (including
  * ValidationException) thrown during deserialization and produce
  * UnparsedErgoTree. The decision about soft-fork can be done later.
  * But is looks like this is not necessary if we do as described below.
  *
  * 2) we can also strictly check during deserialization the content of the script
  * against version number in the header. Thus if the header have vK, then
  * script is allowed to have instructions from versions from v1 to vK. On a node vN, N >
  * K, this should also be enforced, i.e. vN node will reject scripts as invalid
  * if the script has vK in header and vK+1 instruction in body.
  *
  * Keeping this in mind, if we have a vN node and a script with vS in its header then:
  * During script deserialization:
  * 1) if vN >= vS then
  *   the node knows all the instructions and should check that only instructions
  *   up to vS are used in the script.
  *   It either parses successfully or throws MalformedScriptException.
  *   If during the process some unknown instruction is encountered (i.e. ValidationException is thrown),
  *   this cannot be a soft-fork, because vN >= vS guarantees that all instructions are known,
  *   thus the script is malformed.
  *
  * 2) if vN < vS then
  *   the vN node is expecting unknown instructions.
  *   If the script is parsed successfully, then
  *     vN subset of the language is used and script is accepted for execution
  *   else if ValidationException is thrown then
  *     UnparsedErgoTree is created, delaying decision about soft-fork until stateful validation.
  *     if bodySize is stored then
  *       script body is skipped and whole TX deserialization continues.
  *     otherwise
  *       we cannot skip the body which leads to whole TX to be rejected (CannotSkipScriptException)
  *   else if some other exception is thrown then
  *     the whole TX is rejected due to said exception.
  *
  * In the stateful context:
  *   if vN >= vS then
  *     we can execute script, but we do additional check
  *     if vS > the current version of protocol (vP) then
  *       the script is rejected as invalid because its version exceeds
  *       the current consensus version of the protocol
  *     else
  *       the script can be executed
  *   if vN < vS then
  *     if we have Right(tree)
  *       the script is executed
  *     if Left(UnparsedErgoTree()) then check soft fork and either execute or throw
  *
  * Proposition:
  *   CannotSkipScriptException can only happen on < 10% of the nodes, which is safe for consensus.
  * Proof.
  *   If follows from the fact that vN >= vS nodes will reject the script
  *   until new vP is upgraded to vS, which means the majority has upgraded to at least vS
  *   Thus, before vP is upgraded to vS, majority reject (either because they cannot parse, or because vP is not actualized)
  *   after that majority accept (however old nodes still reject but they are < 10%)
  * End of proof.
  *
  */
class ErgoTreeSerializer {

  /** Serialize header and constants section only.*/
  private def serializeHeader(ergoTree: ErgoTree, w: SigmaByteWriter): Unit = {
    w.put(ergoTree.header)
    if (ergoTree.isConstantSegregation) {
      val constantSerializer = ConstantSerializer(DeserializationSigmaBuilder)
      w.putUInt(ergoTree.constants.length)
      ergoTree.constants.foreach(c => constantSerializer.serialize(c, w))
    }
  }

  private def serializeWithoutSize(ergoTree: ErgoTree): Array[Byte] = {
    val w = SigmaSerializer.startWriter()
    serializeHeader(ergoTree, w)
    assert(ergoTree.isRightParsed, s"Right parsed ErgoTree expected: $ergoTree")
    ValueSerializer.serialize(ergoTree.root.right.get, w)
    w.toBytes
  }

  /** Default serialization of ErgoTree.
    * Doesn't apply any transformations and guarantee to preserve original
    * structure after deserialization. */
  def serializeErgoTree(ergoTree: ErgoTree): Array[Byte] = {
    val res = ergoTree.root match {
      case Left(UnparsedErgoTree(bytes, error)) => bytes.array
      case _ =>
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
    res
  }

  /** Default deserialization of ErgoTree (should be inverse to `serializeErgoTree`).
    * Doesn't apply any transformations to the parsed tree. */
  def deserializeErgoTree(bytes: Array[Byte]): ErgoTree  = {
    val r = SigmaSerializer.startReader(bytes)
    deserializeErgoTree(r)
  }

  def deserializeErgoTree(r: SigmaByteReader): ErgoTree = {
    val startPos = r.position
    val (h, sizeOpt) = deserializeHeaderAndSize(r)
    val bodyPos = r.position
    val tree = try {
      val cs = deserializeConstants(h, r)
      val previousConstantStore = r.constantStore
      r.constantStore = new ConstantStore(cs)
      // reader with constant store attached is required (to get tpe for a constant placeholder)
      val root = ValueSerializer.deserialize(r)
      CheckDeserializedScriptIsSigmaProp(root) {}
      r.constantStore = previousConstantStore
      ErgoTree(h, cs, root.asSigmaProp)
    }
    catch {
      case ve: ValidationException =>
        sizeOpt match {
          case Some(treeSize) =>
            val numBytes = bodyPos - startPos + treeSize
            r.position = startPos
            val bytes = r.getBytes(numBytes)
            ErgoTree(ErgoTree.DefaultHeader, EmptyConstants, Left(UnparsedErgoTree(bytes, ve)))
          case None =>
            throw new SerializerException(
              s"Cannot handle ValidationException, ErgoTree serialized without size bit.", None, Some(ve))
        }
    }
    tree
  }

  /** Deserialize `header` and optional `size` slots only. */
  private def deserializeHeaderAndSize(r: SigmaByteReader): (Byte, Option[Int]) = {
    val header = r.getByte()
    CheckHeaderSizeBit(header)
    val sizeOpt = if (ErgoTree.hasSize(header)) {
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
                          newVals: Array[Value[SType]])(implicit vs: SigmaValidationSettings): Array[Byte] = {
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
