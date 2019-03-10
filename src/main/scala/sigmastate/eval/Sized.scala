package sigmastate.eval

import scalan.{Nullable, RType}
import special.collection.{CSizePrim, CSizePair, Size, CSizeOption, CollType, Coll, CSizeColl}
import scalan.RType._
import sigmastate._
import sigmastate.SBigInt.MaxSizeInBytes
import special.sigma._
import SType.AnyOps
import sigmastate.interpreter.CryptoConstants

trait Sized[T] {
    def size(x: T): Size[T]
}
trait SizedLowPriority {
  implicit def collIsSized[T: Sized: RType]: Sized[Coll[T]] = (xs: Coll[T]) => new CSizeColl(xs.map(Sized[T].size))
}
object Sized extends SizedLowPriority {
  def apply[T](implicit sz: Sized[T]): Sized[T] = sz
  def sizeOf[T: Sized](x: T): Size[T] = Sized[T].size(x)

  val SizeBoolean: Size[Boolean] = new CSizePrim(1L, BooleanType)
  val SizeByte: Size[Byte] = new CSizePrim(1L, ByteType)
  val SizeShort: Size[Short] = new CSizePrim(2L, ShortType)
  val SizeInt: Size[Int] = new CSizePrim(4L, IntType)
  val SizeLong: Size[Long] = new CSizePrim(8L, LongType)

  implicit val BooleanIsSized: Sized[Boolean] = (x: Boolean) => SizeBoolean
  implicit val ByteIsSized: Sized[Byte] = (x: Byte) => SizeByte
  implicit val ShortIsSized: Sized[Short] = (x: Short) => SizeShort
  implicit val IntIsSized: Sized[Int] = (x: Int) => SizeInt
  implicit val LongIsSized: Sized[Long] = (x: Long) => SizeLong
  implicit val BigIntIsSized: Sized[BigInt] = (x: BigInt) => new CSizePrim(MaxSizeInBytes, BigIntRType)
  implicit val AvlTreeIsSized: Sized[AvlTree] = (x: AvlTree) => new CSizePrim(AvlTreeData.TreeDataSize, AvlTreeRType)

  private def typeToSized[T](t: RType[T]): Sized[T] = (t match {
    case BooleanType => Sized[Boolean]
    case ByteType => Sized[Byte]
    case ct: CollType[a] => collIsSized(typeToSized(ct.tItem), ct.tItem)
    case _ => sys.error(s"Don't know how to compute Sized for type $t")
  }).asInstanceOf[Sized[T]]

  implicit val AnyValueIsSized: Sized[AnyValue] = (x: AnyValue) => {
    val size = if (x.value == null)
      new CSizePrim[Any](0L, AnyType)
    else {
      val sized = typeToSized(x.tVal)
      sized.size(x.value)
    }
    new CSizeAnyValue(x.tVal, size)
  }

  implicit val CollByteIsSized: Sized[Coll[Byte]] = (xs: Coll[Byte]) => {
    new CSizeColl(Colls.replicate(xs.length, SizeByte))
  }

  private def sizeOfAnyValue(v: AnyValue): Size[Option[AnyValue]] = {
    val size = sizeOf(v)
    new CSizeOption[AnyValue](Some(size))
  }

  private def sizeOfRegisters(b: Box): Size[Coll[Option[AnyValue]]] = {
    new CSizeColl(b.registers.map(sizeOfAnyValue))
  }

  private def sizeOfTokens(b: Box): Size[Coll[(Coll[Byte], Long)]] = {
    val sId = new CSizeColl(Colls.replicate(CryptoConstants.hashLength, SizeByte))
    val sToken = new CSizePair(sId, SizeLong)
    new CSizeColl(Colls.replicate(b.tokens.length, sToken))
  }

  implicit val boxIsSized: Sized[Box] = (b: Box) => {
    new CSizeBox(
      sizeOf(b.propositionBytes),
      sizeOf(b.bytes),
      sizeOf(b.bytesWithoutRef),
      sizeOfRegisters(b),
      sizeOfTokens(b)
      )
  }
  implicit val headerIsSized: Sized[Header] = (b: Header) => new CSizePrim(SHeader.dataSize(0.asWrappedType), HeaderRType)
  implicit val preHeaderIsSized: Sized[PreHeader] = (b: PreHeader) => new CSizePrim(SPreHeader.dataSize(0.asWrappedType), PreHeaderRType)
  implicit val contextIsSized: Sized[Context] = (ctx: Context) => {
    val outputs = sizeOf(ctx.OUTPUTS)
    val inputs = sizeOf(ctx.INPUTS)
    val dataInputs = sizeOf(ctx.dataInputs)
    val selfBox = sizeOf(ctx.SELF)
    val rootHash = sizeOf(ctx.LastBlockUtxoRootHash)
    val headers = sizeOf(ctx.headers)
    val preHeader = sizeOf(ctx.preHeader)
    val vars = ctx.vars.map(sizeOf(_))
    new CSizeContext(outputs, inputs, dataInputs, selfBox, rootHash, headers, preHeader, vars)
  }

}


