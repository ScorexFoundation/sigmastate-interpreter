import sigmastate.Values.Value
import sigmastate.lang.CheckingSigmaBuilder

package object sigmastate {
  import CheckingSigmaBuilder._

  /** Represents cost estimation computed by JITC interpreter.
    * The JITC costs use 10x more accurate scale comparing to block cost values.
    * @see toBlockCost
    */
  class JitCost private[sigmastate] (private[sigmastate] val value: Int) extends AnyVal {
    /** Adds two cost values. */
    def + (y: JitCost): JitCost =
      new JitCost(java7.compat.Math.addExact(value, y.value))

    /** Scales JitCost back to block cost value. */
    def toBlockCost: Int = value / 10
  }
  object JitCost {
    /** Scales the given block cost to the JitCost scale. */
    def fromBlockCost(blockCost: Int): JitCost =
      new JitCost(java7.compat.Math.multiplyExact(blockCost, 10))
  }

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
