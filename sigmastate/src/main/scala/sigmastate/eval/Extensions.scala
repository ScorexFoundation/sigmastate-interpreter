package sigmastate.eval

import org.bouncycastle.math.ec.ECPoint

import java.math.BigInteger
import scalan.RType
import sigmastate.{SCollection, SCollectionType, SType}
import sigmastate.Values.{Constant, ConstantNode}
import sigmastate.lang.CheckingSigmaBuilder
import special.collection.Coll
import special.sigma._
import sigmastate.SType.AnyOps
import org.ergoplatform.ErgoBox
import spire.syntax.all._

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
    @inline def toColl: Coll[T] = Colls.fromArray(arr)
  }

  implicit class EvalIterableOps[T: RType](seq: Iterable[T]) {
    @inline def toColl: Coll[T] = Colls.fromArray[T](seq.toArray(RType[T].classTag))
  }

  implicit class EvalCollOps[T](val coll: Coll[T]) extends AnyVal {
    def foreach(f: T => Unit) = {
      val limit = coll.length
      cfor(0)(_ < limit, _ + 1) { i =>
        f(coll(i))
      }
    }

    /** Helper type synonym. */
    type ElemTpe = SType { type WrappedType = T}

    /** Wraps the collection into ConstantNode using collection's element type. */
    def toConstant: Constant[SCollection[ElemTpe]] = {
      val elemTpe = Evaluation.rtypeToSType(coll.tItem).asInstanceOf[ElemTpe]
      ConstantNode[SCollection[ElemTpe]](coll, SCollectionType(elemTpe))
    }
  }

  // NOTE: it cannot extend AnyVal because of compiler error: type parameter of value class may not be specialized
  implicit class PairCollOps[@specialized A, @specialized B](val coll: Coll[(A,B)]) {
    def foreach(f: (A, B) => Unit) = {
      val (as, bs) = Colls.unzip(coll)
      val limit = coll.length
      cfor(0)(_ < limit, _ + 1) { i =>
        f(as(i), bs(i))
      }
    }
  }

  implicit class DslDataOps[A](data: A)(implicit tA: RType[A]) {
    def toTreeData: Constant[SType] = {
      CheckingSigmaBuilder.mkConstant(data.asWrappedType, Evaluation.rtypeToSType(tA))
    }
  }

  def toAnyValue[A:RType](x: A) = new CAnyValue(x, RType[A].asInstanceOf[RType[Any]])

  implicit class ErgoBoxOps(val ebox: ErgoBox) extends AnyVal {
    def toTestBox(isCost: Boolean): Box = {
      /* NOHF PROOF:
      Changed: removed check for ebox == null
      Motivation: box cannot be null
      Safety: used in ErgoLikeContext where boxes cannot be null
      Examined ergo code: all that leads to ErgoLikeContext creation.
      */
      CostingBox(isCost, ebox)
    }
  }

  def showECPoint(p: ECPoint): String = {
    if (p.isInfinity) {
      "INF"
    }
    else {
      val rawX = p.getRawXCoord.toString.substring(0, 6)
      val rawY = p.getRawYCoord.toString.substring(0, 6)
      s"ECPoint($rawX,$rawY,...)"
    }
  }

  implicit class GroupElementOps(val source: GroupElement) extends AnyVal {
    def showToString: String = showECPoint(source.asInstanceOf[CGroupElement].wrappedValue)
  }


}
