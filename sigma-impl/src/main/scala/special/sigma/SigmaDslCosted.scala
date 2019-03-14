package special.sigma

import special.collection._
import scalan.{RType, NeverInline}

class CSizeAnyValue(val tVal: RType[Any], val valueSize: Size[Any]) extends SizeAnyValue {
  @NeverInline
  override def dataSize: Long = valueSize.dataSize
}

class CSizeSigmaProp(val propBytes: Size[Coll[Byte]]) extends SizeSigmaProp {
  @NeverInline
  override def dataSize: Long = propBytes.dataSize
}

class CSizeBox(
    val propositionBytes: Size[Coll[Byte]],
    val bytes: Size[Coll[Byte]],
    val bytesWithoutRef: Size[Coll[Byte]],
    val registers: Size[Coll[Option[AnyValue]]],
    val tokens: Size[Coll[(Coll[Byte], Long)]]
) extends SizeBox {
  @NeverInline
  override def dataSize: Long = {
    // since `bytes` already contains all serialized data we just return it here
    // however for cost estimation this size is not equal to the sum of the components
    // and we need each component size independently
    bytes.dataSize
  }

  @NeverInline
  override def getReg[T](id: Byte)(implicit tT: RType[T]): Size[Option[T]] = {
    sys.error(s"Shouldn't be called and must be overriden by the class in sigmastate.eval package")
  }
}

class CSizeContext(
    val outputs: Size[Coll[Box]],
    val inputs: Size[Coll[Box]],
    val dataInputs: Size[Coll[Box]],
    val selfBox: Size[Box],
    val lastBlockUtxoRootHash: Size[AvlTree],
    val headers: Size[Coll[Header]],
    val preHeader: Size[PreHeader],
    val vars: Coll[Size[AnyValue]]
) extends SizeContext {
  @NeverInline
  override def dataSize: Long = {
    outputs.dataSize + inputs.dataSize + dataInputs.dataSize +
        lastBlockUtxoRootHash.dataSize + headers.dataSize + preHeader.dataSize +
        33L // minerPubKey
  }

  @NeverInline
  override def getVar[T](id: Byte)(implicit tT: RType[T]): Size[Option[T]] = {
    val varSize = vars(id.toInt).asInstanceOf[SizeAnyValue]
    assert(varSize.tVal == tT, s"Unexpected context variable type found ${varSize.tVal}: expected $tT")
    val foundSize = varSize.valueSize.asInstanceOf[Size[T]]
    new CSizeOption[T](Some(foundSize))
  }
}

class CSizeBuilder extends SizeBuilder {
  def mkSizeAnyValue(tVal: RType[Any], valueSize: Size[Any]): SizeAnyValue = new CSizeAnyValue(tVal, valueSize)

  def mkSizeBox(propositionBytes: Size[Coll[Byte]], bytes: Size[Coll[Byte]],
      bytesWithoutRef: Size[Coll[Byte]], registers: Size[Coll[Option[AnyValue]]],
      tokens: Size[Coll[(Coll[Byte], Long)]]): SizeBox = {
    new CSizeBox(propositionBytes, bytes, bytesWithoutRef, registers, tokens)
  }

  def mkSizeContext(outputs: Size[Coll[Box]],
      inputs: Size[Coll[Box]],
      dataInputs: Size[Coll[Box]],
      selfBox: Size[Box],
      lastBlockUtxoRootHash: Size[AvlTree],
      headers: Size[Coll[Header]],
      preHeader: Size[PreHeader],
      vars: Coll[Size[AnyValue]]): SizeContext =
    new CSizeContext(outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader, vars)
}
