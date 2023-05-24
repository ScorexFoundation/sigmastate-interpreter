package sigmastate.eval

import java.math.BigInteger
import scalan.RType
import sigmastate.{SCollection, SCollectionType, SType}
import sigmastate.Values.{Constant, ConstantNode}
import sigmastate.lang.CheckingSigmaBuilder
import special.collection.Coll
import special.sigma._
import sigmastate.SType.AnyOps
import org.ergoplatform.ErgoBox
import debox.{Buffer => DBuffer}
import debox.cfor
import org.ergoplatform.ErgoBox.TokenId
import sigmastate.crypto.{CryptoFacade, Ecp}

object Extensions {
  private val Colls = CostingSigmaDslBuilder.Colls

  implicit class ByteExt(val b: Byte) extends AnyVal {
    @inline def toBigInt: BigInt = CostingSigmaDslBuilder.BigInt(BigInteger.valueOf(b.toLong))
  }

  implicit class IntExt(val x: Int) extends AnyVal {
    /** Convert this value to BigInt. */
    @inline def toBigInt: BigInt = CostingSigmaDslBuilder.BigInt(BigInteger.valueOf(x.toLong))
  }

  implicit class LongExt(val x: Long) extends AnyVal {
    /** Convert this value to BigInt. */
    @inline def toBigInt: BigInt = CostingSigmaDslBuilder.BigInt(BigInteger.valueOf(x))
  }

  implicit class ArrayOps[T: RType](arr: Array[T]) {
    /** Wraps array into Coll instance. The source array in not cloned. */
    @inline def toColl: Coll[T] = Colls.fromArray(arr)
  }

  /** Extension methods for `Coll[Byte]` not available for generic `Array[T]`. */
  implicit class ArrayByteOps(val arr: Array[Byte]) extends AnyVal {
    /** Wraps array into TokenId instance. The source array in not cloned. */
    @inline def toTokenId: TokenId = Digest32Coll @@ Colls.fromArray(arr)
  }

  implicit class EvalIterableOps[T: RType](seq: Iterable[T]) {
    @inline def toColl: Coll[T] = Colls.fromArray[T](seq.toArray(RType[T].classTag))
  }

  implicit class EvalCollOps[T](val coll: Coll[T]) extends AnyVal {
    /** Helper type synonym. */
    type ElemTpe = SType { type WrappedType = T}

    /** Wraps the collection into ConstantNode using collection's element type. */
    def toConstant: Constant[SCollection[ElemTpe]] = {
      val elemTpe = Evaluation.rtypeToSType(coll.tItem).asInstanceOf[ElemTpe]
      ConstantNode[SCollection[ElemTpe]](coll, SCollectionType(elemTpe))
    }
  }

  implicit class DslDataOps[A](data: A)(implicit tA: RType[A]) {
    def toTreeData: Constant[SType] = {
      CheckingSigmaBuilder.mkConstant(data.asWrappedType, Evaluation.rtypeToSType(tA))
    }
  }

  def toAnyValue[A:RType](x: A) = new CAnyValue(x, RType[A].asInstanceOf[RType[Any]])

  implicit class ErgoBoxOps(val ebox: ErgoBox) extends AnyVal {
    def toTestBox: Box = {
      /* NOHF PROOF:
      Changed: removed check for ebox == null
      Motivation: box cannot be null
      Safety: used in ErgoLikeContext where boxes cannot be null
      Examined ergo code: all that leads to ErgoLikeContext creation.
      */
      CostingBox(ebox)
    }
  }

  /** Shortened String representation of `source` GroupElement. */
  def showECPoint(p: Ecp): String = {
    if (p.isIdentity) {
      "IDENTITY"
    }
    else {
      CryptoFacade.showPoint(p)
    }
  }

  implicit class EcpOps(val source: Ecp) extends AnyVal {
    /** Extracts [[GroupElement]] from the Ecp instance. */
    def toGroupElement: GroupElement = SigmaDsl.GroupElement(source)
  }

  implicit class GroupElementOps(val source: GroupElement) extends AnyVal {
    /** Shortened String representation of `source` GroupElement. */
    def showToString: String = showECPoint(source.asInstanceOf[CGroupElement].wrappedValue)
  }

  implicit class DBufferOps[A](val buf: DBuffer[A]) extends AnyVal {
    /** Sum all values in `buf` using the given Numeric. */
    def sumAll(implicit n: Numeric[A]): A = {
      val limit = buf.length
      var result: A = n.zero
      cfor(0)(_ < limit, _ + 1) { i =>
        result = n.plus(result, buf.elems(i))
      }
      result
    }
  }
}
