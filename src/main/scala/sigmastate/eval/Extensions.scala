package sigmastate.eval

import java.math.BigInteger

import scalan.RType
import scalan.RType._
import sigmastate.{SHeader, SPreHeader, SType, SByte}
import sigmastate.Values.Constant
import sigmastate.lang.DefaultSigmaBuilder
import special.collection.{CSizePrim, Size, CSizeOption, Coll, CSizeColl}
import special.sigma._
import SType.AnyOps
import spire.syntax.all._
import supertagged.{Tagged}

object Extensions {
  private val Colls = CostingSigmaDslBuilder.Colls

  implicit class ByteExt(val b: Byte) extends AnyVal {
    @inline def toBigInt: BigInt = CostingSigmaDslBuilder.BigInt(BigInteger.valueOf(b.toLong))
  }

  implicit class IntExt(val x: Int) extends AnyVal {
    /** Convert this value to BigInt. */
    @inline def toBigInt: BigInt = CostingSigmaDslBuilder.BigInt(BigInteger.valueOf(x.toLong))
  }

  implicit class ArrayOps[T: RType](arr: Array[T]) {
    @inline def toColl: Coll[T] = Colls.fromArray(arr)
  }

//  implicit class TaggedArrayOps[T: RType, U](arr: Tagged[Array[T], U]) {
//    @inline def toColl: Coll[T] = Colls.fromArray(arr)
//  }

  implicit class IterableOfTaggedOps[T: RType, U](seq: Iterable[Tagged[T, U]]) {
    @inline def toColl: Coll[T] = Colls.fromArray[T](seq.toArray(RType[T].classTag))
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
  }

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
      val treeType = Evaluation.toErgoTreeType(tA)
      val treeData = Evaluation.fromDslData(data, tRes = treeType)
      DefaultSigmaBuilder.mkConstant(treeData.asWrappedType, Evaluation.rtypeToSType(tA))
    }
  }

  def toAnyValue[A:RType](x: A) = new TestValue(x, RType[A].asInstanceOf[RType[Any]])
}
