package sigmastate.eval

import debox.cfor
import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.TokenId
import scorex.crypto.authds.avltree.batch.{Insert, Lookup, Remove, Update}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.util.encode.Base16
import sigma.ast.SType.AnyOps
import sigma.data.{CAnyValue, Digest32Coll, Nullable, RType, SigmaBoolean}
import sigma.{Coll, _}
import sigmastate.Platform
import sigma.ast.{SigmaPropIsProven, _}
import sigma.ast.defs._
import sigmastate.interpreter.Interpreter
import sigmastate.lang.{CheckingSigmaBuilder, TransformingSigmaBuilder}
import sigmastate.utils.Helpers
import java.math.BigInteger
import scala.util.{Failure, Success}

object Extensions {

  implicit class ByteExt(val b: Byte) extends AnyVal {
    @inline def toBigInt: BigInt = CSigmaDslBuilder.BigInt(BigInteger.valueOf(b.toLong))
  }

  implicit class ShortExt(val b: Short) extends AnyVal {
    @inline def toBigInt: BigInt = CSigmaDslBuilder.BigInt(BigInteger.valueOf(b.toLong))
  }

  implicit class IntExt(val x: Int) extends AnyVal {
    /** Convert this value to BigInt. */
    @inline def toBigInt: BigInt = CSigmaDslBuilder.BigInt(BigInteger.valueOf(x.toLong))
  }

  implicit class LongExt(val x: Long) extends AnyVal {
    /** Convert this value to BigInt. */
    @inline def toBigInt: BigInt = CSigmaDslBuilder.BigInt(BigInteger.valueOf(x))
  }

  /** Extension methods for `Array[Byte]` not available for generic `Array[T]`. */
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
      CBox(ebox)
    }
  }

  implicit class SigmaBooleanOps(val sb: SigmaBoolean) extends AnyVal {
    def toSigmaPropValue: SigmaPropValue = SigmaPropConstant(sb)

    def isProven: Value[SBoolean.type] = SigmaPropIsProven(SigmaPropConstant(sb))
  }

  implicit class AvlTreeOps(val tree: AvlTree) extends AnyVal {

    def contains(key: Coll[Byte], proof: Coll[Byte]): Boolean = {
      val keyBytes = key.toArray
      val bv       = AvlTreeVerifier(tree, proof)
      bv.performOneOperation(Lookup(ADKey @@ keyBytes)) match {
        case Success(r) => r match {
          case Some(_) => true
          case _ => false
        }
        case Failure(_) => false
      }
    }

    def get(key: Coll[Byte], proof: Coll[Byte]): Option[Coll[Byte]] = {
      val keyBytes = key.toArray
      val bv       = AvlTreeVerifier(tree, proof)
      bv.performOneOperation(Lookup(ADKey @@ keyBytes)) match {
        case Success(r) => r match {
          case Some(v) => Some(Colls.fromArray(v))
          case _ => None
        }
        case Failure(_) => Interpreter.error(s"Tree proof is incorrect $tree")
      }
    }

    def getMany(
        keys: Coll[Coll[Byte]],
        proof: Coll[Byte]): Coll[Option[Coll[Byte]]] = {
      val bv = AvlTreeVerifier(tree, proof)
      keys.map { key =>
        bv.performOneOperation(Lookup(ADKey @@ key.toArray)) match {
          case Success(r) => r match {
            case Some(v) => Some(Colls.fromArray(v))
            case _ => None
          }
          case Failure(_) => Interpreter.error(s"Tree proof is incorrect $tree")
        }
      }
    }

    def insert(
        entries: Coll[(Coll[Byte], Coll[Byte])],
        proof: Coll[Byte]): Option[AvlTree] = {
      if (!tree.isInsertAllowed) {
        None
      } else {
        val bv = AvlTreeVerifier(tree, proof)
        entries.forall { case (key, value) =>
          val insertRes = bv.performOneOperation(Insert(ADKey @@ key.toArray, ADValue @@ value.toArray))
          if (insertRes.isFailure) {
            Interpreter.error(s"Incorrect insert for $tree (key: $key, value: $value, digest: ${tree.digest}): ${insertRes.failed.get}}")
          }
          insertRes.isSuccess
        }
        bv.digest match {
          case Some(d) => Some(tree.updateDigest(Colls.fromArray(d)))
          case _ => None
        }
      }
    }

    def update(
        operations: Coll[(Coll[Byte], Coll[Byte])],
        proof: Coll[Byte]): Option[AvlTree] = {
      if (!tree.isUpdateAllowed) {
        None
      } else {
        val bv = AvlTreeVerifier(tree, proof)
        operations.forall { case (key, value) =>
          bv.performOneOperation(Update(ADKey @@ key.toArray, ADValue @@ value.toArray)).isSuccess
        }
        bv.digest match {
          case Some(d) => Some(tree.updateDigest(Colls.fromArray(d)))
          case _ => None
        }
      }
    }

    def remove(operations: Coll[Coll[Byte]], proof: Coll[Byte]): Option[AvlTree] = {
      if (!tree.isRemoveAllowed) {
        None
      } else {
        val bv = AvlTreeVerifier(tree, proof)
        cfor(0)(_ < operations.length, _ + 1) { i =>
          val key = operations(i).toArray
          bv.performOneOperation(Remove(ADKey @@ key))
        }
        bv.digest match {
          case Some(v) => Some(tree.updateDigest(Colls.fromArray(v)))
          case _ => None
        }
      }
    }
  }
}
