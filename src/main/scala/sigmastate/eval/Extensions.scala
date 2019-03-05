package sigmastate.eval

import java.math.BigInteger

import scalan.RType
import sigmastate.SType
import sigmastate.Values.Constant
import sigmastate.lang.DefaultSigmaBuilder
import special.collection.Coll
import special.sigma._

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

  import SType.AnyOps
  implicit class DslDataOps[A](data: A)(implicit tA: RType[A]) {
    def toTreeData: Constant[SType] = {
      val treeType = Evaluation.toErgoTreeType(tA)
      val treeData = Evaluation.fromDslData(data, tRes = treeType)
      DefaultSigmaBuilder.mkConstant(treeData.asWrappedType, Evaluation.rtypeToSType(tA))
    }
  }

}
