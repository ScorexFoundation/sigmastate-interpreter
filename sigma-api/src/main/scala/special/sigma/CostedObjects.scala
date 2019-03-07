package special.sigma

import special.collection._
import scalan._

@scalan.Liftable
trait SizeAnyValue extends Size[AnyValue] {
  def tVal: RType[Any]
  def valueSize: Size[Any]
}

@scalan.Liftable
trait SizeBox extends Size[Box] {
  def propositionBytes: Size[Coll[Byte]]
  def bytes: Size[Coll[Byte]]
  def bytesWithoutRef: Size[Coll[Byte]]
  def registers: Size[Coll[Option[AnyValue]]]
}

@scalan.Liftable
trait SizeContext extends Size[Context] {
  def outputs: Size[Coll[Box]]
  def inputs: Size[Coll[Box]]
  def dataInputs: Size[Coll[Box]]
  def selfBox: Size[Box]
  def lastBlockUtxoRootHash: Size[AvlTree]
  def headers: Size[Coll[Header]]
  def preHeader: Size[PreHeader]
}

@scalan.Liftable
trait SizeBuilder {
  def mkSizeAnyValue(tVal: RType[Any], valueSize: Size[Any]): SizeAnyValue

  def mkSizeBox(propositionBytes: Size[Coll[Byte]], bytes: Size[Coll[Byte]],
                bytesWithoutRef: Size[Coll[Byte]], registers: Size[Coll[Option[AnyValue]]]): SizeBox

  def mkSizeContext(outputs: Size[Coll[Box]],
                    inputs: Size[Coll[Box]],
                    dataInputs: Size[Coll[Box]],
                    selfBox: Size[Box],
                    lastBlockUtxoRootHash: Size[AvlTree],
                    headers: Size[Coll[Header]],
                    preHeader: Size[PreHeader]): SizeContext
}


