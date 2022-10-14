import scalan.reflection.ReflectionData.registerClassEntry
import scalan.reflection.SRConstructor
import sigmastate.Values.Value
import sigmastate.lang.CheckingSigmaBuilder

package object sigmastate {
  import CheckingSigmaBuilder._

  /** Represents cost estimation computed by JITC interpreter.
    * The JITC costs use 10x more accurate scale comparing to block cost values.
    * @see toBlockCost
    */
  case class JitCost private[sigmastate] (private[sigmastate] val value: Int) extends AnyVal {
    /** Adds two cost values. */
    def + (y: JitCost): JitCost =
      new JitCost(java7.compat.Math.addExact(value, y.value))

    /** Multiplies this cost to the given integer. */
    def * (n: Int): JitCost =
      new JitCost(java7.compat.Math.multiplyExact(value, n))

    /** Divides this cost by the given integer. */
    def / (n: Int): JitCost =
      new JitCost(value / n)

    /** Return true if this value > y.value in the normal Int ordering. */
    def > (y: JitCost): Boolean = value > y.value

    /** Return true if this value >= y.value in the normal Int ordering. */
    def >= (y: JitCost): Boolean = value >= y.value

    /** Scales JitCost back to block cost value. This is inverse to JitCost.fromBlockCost. */
    def toBlockCost: Int = value / 10
  }
  object JitCost {
    /** Scales the given block cost to the JitCost scale. This is inverse to toBlockCost*/
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

  registerClassEntry(classOf[sigmastate.AND],
    constructors = Array(
      new SRConstructor[Any](Array(classOf[sigmastate.Values.Value[_]])) {
        override def newInstance(args: AnyRef*): Any =
          new sigmastate.AND(args(0).asInstanceOf[sigmastate.Values.Value[SCollection[SBoolean.type]]])
      }
    )
  )
}
