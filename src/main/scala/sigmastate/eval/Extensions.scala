package sigmastate.eval

import java.math.BigInteger

import scalan.RType
import scalan.RType._
import sigmastate.{SHeader, SType, SByte, SPreHeader}
import sigmastate.Values.Constant
import sigmastate.lang.DefaultSigmaBuilder
import special.collection.{CSizePrim, Size, CSizeOption, Coll, CSizeColl}
import special.sigma._
import SType.AnyOps

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

  implicit class DslDataOps[A](data: A)(implicit tA: RType[A]) {
    def toTreeData: Constant[SType] = {
      val treeType = Evaluation.toErgoTreeType(tA)
      val treeData = Evaluation.fromDslData(data, tRes = treeType)
      DefaultSigmaBuilder.mkConstant(treeData.asWrappedType, Evaluation.rtypeToSType(tA))
    }
  }

  def toAnyValue[A:RType](x: A) = new TestValue(x, RType[A].asInstanceOf[RType[Any]])
}
