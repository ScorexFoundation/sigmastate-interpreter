package special.sigma

import special.SpecialPredef
import special.collection.{Coll, CCostedPrim, _}

import scala.reflect.ClassTag
import scalan.RType
import scalan.{NeverInline, Reified}

class CSizeAnyValue(val tVal: RType[Any], val valueSize: Size[Any]) extends SizeAnyValue {
  @NeverInline
  override def dataSize: Long = valueSize.dataSize
}

class CSizeBox(
    val propositionBytes: Size[Coll[Byte]],
    val bytes: Size[Coll[Byte]],
    val bytesWithoutRef: Size[Coll[Byte]],
    val registers: Size[Coll[Option[AnyValue]]],
) extends SizeBox {
  @NeverInline
  override def dataSize: Long = {
    // since `bytes` already contains all serialized data we just return it here
    // however for cost estimation this size is not equal to the sum of the components
    // and we need each component size independently
    bytes.dataSize
  }
}

class CSizeContext(
    val outputs: Size[Coll[Box]],
    val inputs: Size[Coll[Box]],
    val dataInputs: Size[Coll[Box]],
    val selfBox: Size[Box],
    val lastBlockUtxoRootHash: Size[AvlTree],
    val headers: Size[Coll[Header]],
    val preHeader: Size[PreHeader]
) extends SizeContext {
  @NeverInline
  override def dataSize: Long = {
    outputs.dataSize + inputs.dataSize + dataInputs.dataSize +
        lastBlockUtxoRootHash.dataSize + headers.dataSize + preHeader.dataSize +
        33L // minerPubKey
  }
}

class CSizeBuilder extends SizeBuilder {
  def mkSizeAnyValue(tVal: RType[Any], valueSize: Size[Any]): SizeAnyValue = new CSizeAnyValue(tVal, valueSize)

  def mkSizeBox(propositionBytes: Size[Coll[Byte]], bytes: Size[Coll[Byte]],
      bytesWithoutRef: Size[Coll[Byte]], registers: Size[Coll[Option[AnyValue]]]): SizeBox = {
    new CSizeBox(propositionBytes, bytes, bytesWithoutRef, registers)
  }

  def mkSizeContext(outputs: Size[Coll[Box]],
      inputs: Size[Coll[Box]],
      dataInputs: Size[Coll[Box]],
      selfBox: Size[Box],
      lastBlockUtxoRootHash: Size[AvlTree],
      headers: Size[Coll[Header]],
      preHeader: Size[PreHeader]): SizeContext =
    new CSizeContext(outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader)
}
