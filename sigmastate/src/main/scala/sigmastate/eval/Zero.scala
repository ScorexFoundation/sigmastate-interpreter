package sigmastate.eval

import java.math.BigInteger

import scalan.{Nullable, RType}
import special.collection.{CSizePrim, CSizePair, Size, CSizeOption, CollType, Coll, CSizeColl}
import scalan.RType._
import sigmastate._
import sigmastate.SBigInt.MaxSizeInBytes
import special.sigma._
import SType.AnyOps
import scorex.crypto.authds.avltree.batch.BatchAVLProver
import scorex.crypto.hash.{Digest32, Blake2b256}
import sigmastate.interpreter.CryptoConstants
import sigmastate.interpreter.CryptoConstants.EcPointType

trait Zero[T] {
    def zero: T
}

case class CZero[T](zero: T) extends Zero[T]

trait ZeroLowPriority {
  implicit def collIsZero[T: Zero: RType]: Zero[Coll[T]] = CZero(Colls.emptyColl[T])
  implicit def optionIsZero[T: Zero]: Zero[Option[T]] = CZero(Some(Zero.zeroOf[T]))
  implicit def pairIsZero[A: Zero, B: Zero]: Zero[(A,B)] = CZero(Zero[A].zero, Zero[B].zero)
}
object Zero extends ZeroLowPriority {
  def apply[T](implicit z: Zero[T]): Zero[T] = z
  def zeroOf[T: Zero]: T = Zero[T].zero

  implicit val BooleanIsZero: Zero[Boolean] = CZero(false)
  implicit val ByteIsZero: Zero[Byte] = CZero(0.toByte)
  implicit val ShortIsZero: Zero[Short] = CZero(0.toShort)
  implicit val IntIsZero: Zero[Int] = CZero(0)
  implicit val LongIsZero: Zero[Long] = CZero(0L)
  implicit val BigIntIsZero: Zero[BigInt] = CZero(CBigInt(BigInteger.ZERO))
  implicit val GroupElementIsZero: Zero[GroupElement] = CZero(CGroupElement(CryptoConstants.dlogGroup.identity))
  implicit val AvlTreeIsZero: Zero[AvlTree] = CZero({
    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)
    val digest = avlProver.digest
    val treeData = new AvlTreeData(digest, AvlTreeFlags.AllOperationsAllowed, 32, None)
    CAvlTree(treeData)
  })

  def typeToZero[T](t: RType[T]): Zero[T] = (t match {
    case BooleanType => Zero[Boolean]
    case ByteType => Zero[Byte]
    case ShortType => Zero[Short]
    case IntType => Zero[Int]
    case LongType => Zero[Long]
    case BigIntRType => Zero[BigInt]
    case GroupElementRType => Zero[GroupElement]
    case AvlTreeRType => Zero[AvlTree]
    case SigmaPropRType => sigmaPropIsZero
    case ct: CollType[a] => collIsZero(typeToZero(ct.tItem), ct.tItem)
    case ct: OptionType[a] => optionIsZero(typeToZero(ct.tA))
    case ct: PairType[a, b] => pairIsZero(typeToZero(ct.tFst), typeToZero(ct.tSnd))
    case _ => sys.error(s"Don't know how to compute Zero for type $t")
  }).asInstanceOf[Zero[T]]

  implicit val sigmaPropIsZero: Zero[SigmaProp] = CZero(CSigmaProp(TrivialProp.FalseProp))
}


