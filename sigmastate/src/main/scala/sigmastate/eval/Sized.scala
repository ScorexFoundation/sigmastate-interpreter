package sigmastate.eval

import scalan.RType
import special.collection._
import scalan.RType._
import sigmastate._
import special.sigma._
import SType.AnyOps
import org.ergoplatform.SigmaConstants.{MaxBoxSize, MaxTokens}
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
  implicit def collIsSized[T: Sized]: Sized[Coll[T]] = Sized.instance((xs: Coll[T]) => {
    implicit val tT = xs.tItem
    val sizes =
      if (xs.isEmpty) Colls.emptyColl[Size[T]]
      else if (xs.tItem.isConstantSize)
        Colls.replicate(xs.length, Sized.sizeOf(xs(0)))
      else
        new CViewColl(xs, Sized[T].size)
    new CSizeColl(sizes)
  })
  implicit def optionIsSized[T: Sized]: Sized[Option[T]] =
    Sized.instance((xs: Option[T]) => new CSizeOption(xs.map(Sized[T].size)))
  implicit def pairIsSized[A: Sized, B: Sized]: Sized[(A,B)] =
    Sized.instance((in: (A,B)) => new CSizePair(Sized[A].size(in._1), Sized[B].size(in._2)))
}

object Sized extends SizedLowPriority {
  def apply[T](implicit sz: Sized[T]): Sized[T] = sz
  def sizeOf[T: Sized](x: T): Size[T] = Sized[T].size(x)
  def instance[T](f: T => Size[T]) = new Sized[T] {
    override def size(x: T): Size[T] = f(x)
  }

  val SizeBoolean: Size[Boolean] = new CSizePrim(1L, BooleanType)
  val SizeByte: Size[Byte] = new CSizePrim(1L, ByteType)
  val SizeShort: Size[Short] = new CSizePrim(2L, ShortType)
  val SizeInt: Size[Int] = new CSizePrim(4L, IntType)
  val SizeLong: Size[Long] = new CSizePrim(8L, LongType)
  val SizeBigInt: Size[BigInt] = new CSizePrim(SBigInt.MaxSizeInBytes, BigIntRType)
  val SizeGroupElement: Size[GroupElement] = new CSizePrim(CryptoConstants.EncodedGroupElementLength, GroupElementRType)
  val SizeSigmaProp: Size[SigmaProp] = new CSizePrim(SSigmaProp.MaxSizeInBytes, SigmaPropRType)
  val SizeAvlTree: Size[AvlTree] = new CSizePrim(AvlTreeData.TreeDataSize, AvlTreeRType)

  implicit val BooleanIsSized: Sized[Boolean] = Sized.instance((_: Boolean) => SizeBoolean)
  implicit val ByteIsSized: Sized[Byte] = Sized.instance((_: Byte) => SizeByte)
  implicit val ShortIsSized: Sized[Short] = Sized.instance((_: Short) => SizeShort)
  implicit val IntIsSized: Sized[Int] = Sized.instance((_: Int) => SizeInt)
  implicit val LongIsSized: Sized[Long] = Sized.instance((_: Long) => SizeLong)
  implicit val BigIntIsSized: Sized[BigInt] = Sized.instance((_: BigInt) => SizeBigInt)
  implicit val GroupElementIsSized: Sized[GroupElement] = Sized.instance((_: GroupElement) => SizeGroupElement)
  implicit val SigmaPropIsSized: Sized[SigmaProp] = Sized.instance((_: SigmaProp) => SizeSigmaProp)
  implicit val AvlTreeIsSized: Sized[AvlTree] = Sized.instance((_: AvlTree) => SizeAvlTree)

  def typeToSized[T](t: RType[T]): Sized[T] = (t match {
    case BooleanType => BooleanIsSized
    case ByteType => ByteIsSized
    case ShortType => ShortIsSized
    case IntType => IntIsSized
    case LongType => LongIsSized
    case BigIntRType => BigIntIsSized
    case GroupElementRType => GroupElementIsSized
    case AvlTreeRType => AvlTreeIsSized
    case SigmaPropRType => SigmaPropIsSized
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

  val SizeNullValue: SizeAnyValue = {
    val size = new CSizePrim[Any](0L, NothingType.asInstanceOf[RType[Any]])
    new CSizeAnyValue(NothingType.asInstanceOf[RType[Any]], size)
  }

  implicit val anyValueIsSized: Sized[AnyValue] = Sized.instance((x: AnyValue) => {
    if (x.tVal == null) {
      SizeNullValue
    } else {
      assert(x.value != null, s"Invalid AnyValue: non-null type ${x.tVal} and null value.")
      val sized = typeToSized(x.tVal)
      val size = sized.size(x.value)
      new CSizeAnyValue(x.tVal, size)
    }
  })

  implicit val CollByteIsSized: Sized[Coll[Byte]] = Sized.instance((xs: Coll[Byte]) => {
    new CSizeColl(Colls.replicate(xs.length, SizeByte))
  })

  private val SizeOptionAnyValueNone = new CSizeOption[AnyValue](None)

  private def sizeOfAnyValue(v: AnyValue): Size[Option[AnyValue]] = {
    if (v == null) return SizeOptionAnyValueNone
    val size = sizeOf(v)
    new CSizeOption[AnyValue](Some(size))
  }

  private def sizeOfRegisters(b: Box): Size[Coll[Option[AnyValue]]] = {
    new CSizeColl(b.registers.map(sizeOfAnyValue))
  }

  val SizeHash = new CSizeColl(Colls.replicate(CryptoConstants.hashLength, SizeByte))
  val SizeCreationInfo: Size[(Int, Coll[Byte])] = new CSizePair(SizeInt, SizeHash)

  val SizeTokenId = new CSizeColl(Colls.replicate(CryptoConstants.hashLength, SizeByte))
  val SizeToken: Size[(Coll[Byte], Long)] = new CSizePair(SizeTokenId, SizeLong)
  val SizeTokensMax = new CSizeColl(Colls.replicate(MaxTokens.value, SizeToken))

  val SizePropositionBytesMax = new CSizeColl(Colls.replicate(4096 /*4K*/, SizeByte))
  val SizeBoxBytesMax = new CSizeColl(Colls.replicate(MaxBoxSize.value, SizeByte))
  val SizeOfInputRefBytes = CryptoConstants.hashLength + SizeShort.dataSize.toInt
  val SizeBoxBytesWithoutRefsMax = new CSizeColl(
    Colls.replicate(MaxBoxSize.value - SizeOfInputRefBytes, SizeByte))

  private def sizeOfTokens(b: Box): Size[Coll[(Coll[Byte], Long)]] = {
    new CSizeColl(Colls.replicate(b.tokens.length, SizeToken))
  }

  implicit val boxIsSized: Sized[Box] = Sized.instance((b: Box) => {
    new EvalSizeBox(
      SizePropositionBytesMax,
      SizeBoxBytesMax,
      SizeBoxBytesWithoutRefsMax,
      sizeOfRegisters(b),
      SizeTokensMax
      )
  })

  val SizeHeader = new CSizePrim(SHeader.dataSize(0.asWrappedType), HeaderRType)
  implicit val headerIsSized: Sized[Header] = Sized.instance((_: Header) => SizeHeader)

  val SizePreHeader = new CSizePrim(SPreHeader.dataSize(0.asWrappedType), PreHeaderRType)
  implicit val preHeaderIsSized: Sized[PreHeader] = Sized.instance((_: PreHeader) => SizePreHeader)

  implicit val contextIsSized: Sized[Context] = Sized.instance((ctx: Context) => {
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
  })

}


