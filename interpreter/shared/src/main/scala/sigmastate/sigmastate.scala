import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, ErgoLikeContext}
import sigma.ast._
import sigma.data.{AvlTreeData, GeneralType, RType}
import sigmastate.Values._
import sigmastate.lang.{CheckingSigmaBuilder, Terms}

import scala.reflect.classTag

package object sigmastate {
  import CheckingSigmaBuilder._

  /** RType descriptors for predefined types used in AOTC-based interpreter. */
  def rtypeToClassTag = ???

  implicit val SigmaBooleanRType    : RType[SigmaBoolean]     = RType.fromClassTag(classTag[SigmaBoolean])

  implicit val ErgoBoxRType         : RType[ErgoBox]          = RType.fromClassTag(classTag[ErgoBox])

  implicit val ErgoBoxCandidateRType: RType[ErgoBoxCandidate] = RType.fromClassTag(classTag[ErgoBoxCandidate])

  implicit val AvlTreeDataRType     : RType[AvlTreeData]      = GeneralType(classTag[AvlTreeData])

  implicit val ErgoLikeContextRType : RType[ErgoLikeContext]  = RType.fromClassTag(classTag[ErgoLikeContext])

  implicit class STypeOps(val tpe: SType) extends AnyVal {
    def isCollectionLike: Boolean = tpe.isInstanceOf[SCollection[_]]

    def isCollection: Boolean = tpe.isInstanceOf[SCollectionType[_]]

    def isOption: Boolean = tpe.isInstanceOf[SOption[_]]

    def isBox: Boolean = tpe.isInstanceOf[SBox.type]

    def isGroupElement: Boolean = tpe.isInstanceOf[SGroupElement.type]

    def isSigmaProp: Boolean = tpe.isInstanceOf[SSigmaProp.type]

    def isAvlTree: Boolean = tpe.isInstanceOf[SAvlTree.type]

    def isFunc: Boolean = tpe.isInstanceOf[SFunc]

    def isTuple: Boolean = tpe.isInstanceOf[STuple]

    /** Returns true if this type is numeric (Byte, Short, etc.)
      *
      * @see [[sigmastate.SNumericType]]
      */
    def isNumType: Boolean = tpe.isInstanceOf[SNumericType]

    /** Returns true if this type is either numeric (Byte, Short, etc.) or is NoType.
      *
      * @see [[sigmastate.SNumericType]]
      */
    def isNumTypeOrNoType: Boolean = isNumType || tpe == NoType

    def asNumType: SNumericType = tpe.asInstanceOf[SNumericType]

    def asFunc: SFunc = tpe.asInstanceOf[SFunc]

    def asProduct: SProduct = tpe.asInstanceOf[SProduct]

    def asTuple: STuple = tpe.asInstanceOf[STuple]

    def asOption[T <: SType]: SOption[T] = tpe.asInstanceOf[SOption[T]]

    def whenFunc[T](action: SFunc => Unit) = if (tpe.isInstanceOf[SFunc]) action(tpe.asFunc)

    def asCollection[T <: SType] = tpe.asInstanceOf[SCollection[T]]

    /** Applies a type substitution to this type.
      *
      * @param subst the type substitution to apply
      * @return the type after applying the substitution
      */
    def withSubstTypes(subst: Map[STypeVar, SType]): SType =
      if (subst.isEmpty) tpe
      else
        Terms.applySubst(tpe, subst)
  }

  /** Helper method to create "+" operation node. */
  def Plus[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkPlus(left, right)

  /** Helper method to create "-" operation node. */
  def Minus[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkMinus(left, right)

  /** Helper method to create "*" operation node. */
  def Multiply[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkMultiply(left, right)

  /** Helper method to create "/" operation node. */
  def Divide[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkDivide(left, right)

  /** Helper method to create "%" operation node. */
  def Modulo[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkModulo(left, right)

  /** Helper method to create "min" operation node. */
  def Min[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkMin(left, right)

  /** Helper method to create "max" operation node. */
  def Max[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkMax(left, right)

}
