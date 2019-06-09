package sigmastate.eval

import scalan.RType
import special.collection._
import scalan.RType._
import sigmastate._
import sigmastate.SBigInt.MaxSizeInBytes
import special.sigma._
import SType.AnyOps
import org.ergoplatform.ErgoConstants.{MaxBoxSize, MaxPropositionBytes, MaxTokens}
import sigmastate.interpreter.CryptoConstants

/** Type-class to give types a capability to build a Size structure. */
trait Sized[T] {
    /** Given data value `x` returns it's size descriptor `Size[T]` */
    def size(x: T): Size[T]
}

trait SizedLowPriority {
  /** Sized instance for Coll[T].
    * Takes advantage of RType.isConstantSize to use ReplColl representation of Coll when all items are the same.
    * When all elements of T are of the same size, then only single Size[T] value is created and replicated
    * to the length of source collection `xs`. */
  implicit def collIsSized[T: Sized]: Sized[Coll[T]] = (xs: Coll[T]) => {
    implicit val tT = xs.tItem
    val sizes =
      if (xs.isEmpty) Colls.emptyColl[Size[T]]
      else if (xs.tItem.isConstantSize)
        Colls.replicate(xs.length, Sized.sizeOf(xs(0)))
      else
        new CViewColl(xs, Sized[T].size)
    new CSizeColl(sizes)
  }
  implicit def optionIsSized[T: Sized]: Sized[Option[T]] = (xs: Option[T]) => new CSizeOption(xs.map(Sized[T].size))
  implicit def pairIsSized[A: Sized, B: Sized]: Sized[(A,B)] = (in: (A,B)) => new CSizePair(Sized[A].size(in._1), Sized[B].size(in._2))
}
object Sized extends SizedLowPriority {
  def apply[T](implicit sz: Sized[T]): Sized[T] = sz
  def sizeOf[T: Sized](x: T): Size[T] = Sized[T].size(x)

  val SizeBoolean: Size[Boolean] = new CSizePrim(1L, BooleanType)
  val SizeByte: Size[Byte] = new CSizePrim(1L, ByteType)
  val SizeShort: Size[Short] = new CSizePrim(2L, ShortType)
  val SizeInt: Size[Int] = new CSizePrim(4L, IntType)
  val SizeLong: Size[Long] = new CSizePrim(8L, LongType)
  val SizeBigInt: Size[BigInt] = new CSizePrim(MaxSizeInBytes, BigIntRType)
  val SizeGroupElement: Size[GroupElement] = new CSizePrim(CryptoConstants.EncodedGroupElementLength, GroupElementRType)
  val SizeAvlTree: Size[AvlTree] = new CSizePrim(AvlTreeData.TreeDataSize, AvlTreeRType)

  implicit val BooleanIsSized: Sized[Boolean] = (_: Boolean) => SizeBoolean
  implicit val ByteIsSized: Sized[Byte] = (_: Byte) => SizeByte
  implicit val ShortIsSized: Sized[Short] = (_: Short) => SizeShort
  implicit val IntIsSized: Sized[Int] = (_: Int) => SizeInt
  implicit val LongIsSized: Sized[Long] = (_: Long) => SizeLong
  implicit val BigIntIsSized: Sized[BigInt] = (_: BigInt) => SizeBigInt
  implicit val GroupElementIsSized: Sized[GroupElement] = (_: GroupElement) => SizeGroupElement
  implicit val AvlTreeIsSized: Sized[AvlTree] = (_: AvlTree) => SizeAvlTree

  def typeToSized[T](t: RType[T]): Sized[T] = (t match {
    case BooleanType => Sized[Boolean]
    case ByteType => Sized[Byte]
    case ShortType => Sized[Short]
    case IntType => Sized[Int]
    case LongType => Sized[Long]
    case BigIntRType => Sized[BigInt]
    case GroupElementRType => Sized[GroupElement]
    case AvlTreeRType => Sized[AvlTree]
    case SigmaPropRType => sigmaPropIsSized
    case AnyValueRType => anyValueIsSized
    case BoxRType => boxIsSized
    case HeaderRType => headerIsSized
    case PreHeaderRType => preHeaderIsSized
    case ContextRType => contextIsSized
    case ct: CollType[a] => collIsSized(typeToSized(ct.tItem))
    case ct: OptionType[a] => optionIsSized(typeToSized(ct.tA))
    case ct: PairType[a, b] => pairIsSized(typeToSized(ct.tFst), typeToSized(ct.tSnd))
    case _ => sys.error(s"Don't know how to compute Sized for type $t")
  }).asInstanceOf[Sized[T]]

  implicit val anyValueIsSized: Sized[AnyValue] = (x: AnyValue) => {
    if (x.tVal == null) {
      val size = new CSizePrim[Any](0L, NothingType.asInstanceOf[RType[Any]])
      new CSizeAnyValue(NothingType.asInstanceOf[RType[Any]], size)
    } else {
      assert(x.value != null, s"Invalid AnyValue: non-null type ${x.tVal} and null value.")
      val sized = typeToSized(x.tVal)
      val size = sized.size(x.value)
      new CSizeAnyValue(x.tVal, size)
    }
  }

  implicit val CollByteIsSized: Sized[Coll[Byte]] = (xs: Coll[Byte]) => {
    new CSizeColl(Colls.replicate(xs.length, SizeByte))
  }

  private def sizeOfAnyValue(v: AnyValue): Size[Option[AnyValue]] = {
    if (v == null) return new CSizeOption[AnyValue](None)
    val size = sizeOf(v)
    new CSizeOption[AnyValue](Some(size))
  }

  private def sizeOfRegisters(b: Box): Size[Coll[Option[AnyValue]]] = {
    new CSizeColl(b.registers.map(sizeOfAnyValue))
  }

  val SizeTokenId = new CSizeColl(Colls.replicate(CryptoConstants.hashLength, SizeByte))
  val SizeToken: Size[(Coll[Byte], Long)] = new CSizePair(SizeTokenId, SizeLong)
  val SizeTokens = new CSizeColl(Colls.replicate(MaxTokens.value.toInt, SizeToken))

  val SizePropositionBytes = new CSizeColl(Colls.replicate(MaxPropositionBytes.value, SizeByte))
  val SizeBoxBytes = new CSizeColl(Colls.replicate(MaxBoxSize.value, SizeByte))
  val SizeBoxBytesWithoutRefs = new CSizeColl(
    Colls.replicate(MaxBoxSize.value - (CryptoConstants.hashLength + 4),
    SizeByte))



  private def sizeOfTokens(b: Box): Size[Coll[(Coll[Byte], Long)]] = {
    new CSizeColl(Colls.replicate(b.tokens.length, SizeToken))
  }

  implicit val sigmaPropIsSized: Sized[SigmaProp] = (b: SigmaProp) => {
    new CSizeSigmaProp(sizeOf(b.propBytes))
  }
  implicit val boxIsSized: Sized[Box] = (b: Box) => {
    new EvalSizeBox(
      SizePropositionBytes,
      SizeBoxBytes,
      SizeBoxBytesWithoutRefs,
      sizeOfRegisters(b),
      SizeTokens
      )
  }

  val SizeHeader = new CSizePrim(SHeader.dataSize(0.asWrappedType), HeaderRType)
  implicit val headerIsSized: Sized[Header] = (_: Header) => SizeHeader

  val SizePreHeader = new CSizePrim(SPreHeader.dataSize(0.asWrappedType), PreHeaderRType)
  implicit val preHeaderIsSized: Sized[PreHeader] = (_: PreHeader) => SizePreHeader

  implicit val contextIsSized: Sized[Context] = (ctx: Context) => {
    val outputs = sizeOf(ctx.OUTPUTS)
    val inputs = sizeOf(ctx.INPUTS)
    val dataInputs = sizeOf(ctx.dataInputs)
    val selfBox = sizeOf(ctx.SELF)
    val rootHash = sizeOf(ctx.LastBlockUtxoRootHash)
    val headers = sizeOf(ctx.headers)
    val preHeader = sizeOf(ctx.preHeader)
    val vars = ctx.vars.map { v =>
      val anyV = if(v == null) TestValue(null, null) else v
      sizeOf(anyV)
    }
    new CSizeContext(outputs, inputs, dataInputs, selfBox, rootHash, headers, preHeader, vars)
  }

}


