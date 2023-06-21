package org.ergoplatform.sdk.utils

import org.ergoplatform.ErgoBox
import scalan.RType
import scalan.RType.{BooleanType, ByteType, FuncType, IntType, LongType, OptionType, PairType, ShortType, UnitType}
import scorex.crypto.authds.avltree.batch.BatchAVLProver
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.ModifierId
import sigmastate.{AvlTreeData, AvlTreeFlags, TrivialProp}
import sigmastate.Values.ErgoTree
import sigmastate.basics.CryptoConstants
import sigmastate.eval.{CAvlTree, CBigInt, CGroupElement, CHeader, CPreHeader, CSigmaProp, Colls, CostingBox, CostingDataContext}
import special.Types.TupleType
import special.collection.{Coll, CollType}
import special.sigma.{AvlTree, AvlTreeRType, BigInt, BigIntRType, Box, BoxRType, Context, ContextRType, GroupElement, GroupElementRType, Header, HeaderRType, PreHeader, PreHeaderRType, SigmaDslBuilder, SigmaDslBuilderRType, SigmaProp, SigmaPropRType}

import java.math.BigInteger
import scalan.RType._
import sigmastate.eval._
import special.sigma._
import scala.language.implicitConversions

/**
 * A trait representing the zero value of each type in the ErgoTree.
 * @tparam T The type of the zero value.
 */
trait Zero[T] {
  /** Get the underlying zero value. */
  def zero: T
}

/**
 * A wrapper over the zero value of a type.
 * @param zero the zero value of the type T.
 * @tparam T The type of the zero value.
 */
case class CZero[T](zero: T) extends Zero[T]

/** A trait providing implicit conversions to create instances of Zero for various types. */
trait ZeroLowPriority {
  implicit def collIsZero[T: Zero: RType]: Zero[Coll[T]] = CZero(Colls.emptyColl[T])
  implicit def optionIsZero[T: Zero]: Zero[Option[T]] = CZero(None)
  implicit def pairIsZero[A: Zero, B: Zero]: Zero[(A,B)] = CZero(Zero[A].zero, Zero[B].zero)
  implicit def funcIsZero[A, B: Zero]: Zero[A =>B] = CZero((_ : A) => { Zero[B].zero })
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
  implicit val sigmaPropIsZero: Zero[SigmaProp] = CZero(CSigmaProp(TrivialProp.FalseProp))
  implicit val UnitIsZero: Zero[Unit] = CZero(())
  implicit val BoxIsZero: Zero[Box] = CZero({
    new ErgoBox(
      LongIsZero.zero,
      new ErgoTree(
        ByteIsZero.zero,
        IndexedSeq.empty,
        Right(sigmaPropIsZero.zero)
      ),
      Colls.emptyColl,
      Map.empty,
      ModifierId @@ ("synthetic_transaction_id"),
      ShortIsZero.zero,
      IntIsZero.zero
    )
  })

  /**
   * Returns the zero value of the specified type `T` using the provided runtime type t.
   * @param t  the runtime type of the value whose zero value is to be returned.
   * @tparam T the type of value whose zero value is to be returned.
   * @return the zero value of type `T`
   * @throws `RuntimeException` if the method is unable to compute the zero value for the specified type
   */
  def typeToZero[T](t: RType[T]): Zero[T] = (t match {
    case BooleanType => Zero[Boolean]
    case ByteType => Zero[Byte]
    case ShortType => Zero[Short]
    case IntType => Zero[Int]
    case LongType => Zero[Long]
    case UnitType => Zero[Unit]
    case BigIntRType => Zero[BigInt]
    case BoxRType => Zero[Box]
    case GroupElementRType => Zero[GroupElement]
    case AvlTreeRType => Zero[AvlTree]
    case SigmaPropRType => sigmaPropIsZero
    case ct: CollType[a] => collIsZero(typeToZero(ct.tItem), ct.tItem)
    case ct: OptionType[a] => optionIsZero(typeToZero(ct.tA))
    case ct: PairType[a, b] => pairIsZero(typeToZero(ct.tFst), typeToZero(ct.tSnd))
    case tt: TupleType => CZero(tt.emptyArray)
    case ft: FuncType[a, b] => funcIsZero(typeToZero(ft.tRange))
    case _ => sys.error(s"Don't know how to compute Zero for type $t")
  }).asInstanceOf[Zero[T]]
}
