package sigmastate.eval

import debox.{cfor, Buffer => DBuffer}
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.TokenId
import scalan.{Nullable, RType}
import scorex.util.encode.Base16
import sigmastate.SType.AnyOps
import sigmastate.Values.{Constant, ConstantNode}
import sigmastate.crypto.{CryptoFacade, Ecp}
import sigmastate.lang.{CheckingSigmaBuilder, TransformingSigmaBuilder}
import sigmastate.utils.Helpers
import sigmastate.{Platform, SCollection, SCollectionType, SType}
import sigma.Coll
import sigma._

import java.math.BigInteger

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
    /** Encodes array into hex string */
    @inline def toHex: String = Base16.encode(arr)
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

    /** Transforms this collection into array of constants.
      *
      * This method have exactly the same semantics on JS and JVM IF `coll.tItem`
      * precisely describes the type of elements in `call`. (Which is the case for all
      * collections created by ErgoTree interpreter).
      *
      * However, if it is not the case, then JVM and JS will have different semantics for Byte and Short.
      *
      * The JVM version preserves v5.0 consensus protocol semantics.
      * The JS version is a reasonable approximation of the JVM version.
      */
    def toArrayOfConstants: Array[Constant[SType]] = {
      val constants = coll.toArray.map { v =>
        // see ScalaDoc for ensureTypeCarringValue
        val valToLift = Helpers.ensureTypeCarringValue(v, coll.tItem.asInstanceOf[RType[Any]])
        // call platform-specific method to transform the value to a Constant
        Platform.liftToConstant(valToLift, TransformingSigmaBuilder) match {
          case Nullable(c) => c
          case _ => sys.error(s"Cannot liftToConstant($valToLift)")
        }
      }
      constants
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
