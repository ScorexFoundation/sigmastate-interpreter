import sigmastate.Values.Value
import sigmastate.lang.DefaultSigmaBuilder

package object sigmastate {
  import DefaultSigmaBuilder._

  /**
    * SInt addition
    */
  def Plus[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkPlus(left, right)

  /**
    * SInt subtraction
    */
  def Minus[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkMinus(left, right)

  /**
    * SInt multiplication
    */
  def Multiply[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkMultiply(left, right)

  /**
    * SInt division
    */
  def Divide[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkDivide(left, right)

  /**
    * SInt modulo
    */
  def Modulo[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkModulo(left, right)

  def Min[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkMin(left, right)

  def Max[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkMax(left, right)

  def PlusModQ(left: Value[SBigInt.type], right: Value[SBigInt.type]): Value[SBigInt.type] =
    mkPlusModQ(left, right)

  def MinusModQ(left: Value[SBigInt.type], right: Value[SBigInt.type]): Value[SBigInt.type] =
    mkMinusModQ(left, right)
}
