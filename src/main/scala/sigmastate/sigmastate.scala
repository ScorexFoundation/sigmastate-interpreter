import sigmastate.Values.Value
import sigmastate.lang.DefaultSigmaBuilder

package object sigmastate {

  /**
    * SInt addition
    */
  def Plus[T <: SNumericType](left: Value[T], right: Value[T]): TwoArgumentsOperation[T,T,T] =
    DefaultSigmaBuilder.Plus(left, right)

  /**
    * SInt subtraction
    */
  def Minus[T <: SNumericType](left: Value[T], right: Value[T]): TwoArgumentsOperation[T,T,T] =
    DefaultSigmaBuilder.Minus(left, right)

  /**
    * SInt multiplication
    */
  def Multiply[T <: SNumericType](left: Value[T], right: Value[T]): TwoArgumentsOperation[T,T,T] =
    DefaultSigmaBuilder.Multiply(left, right)

  /**
    * SInt division
    */
  def Divide[T <: SNumericType](left: Value[T], right: Value[T]): TwoArgumentsOperation[T,T,T] =
    DefaultSigmaBuilder.Divide(left, right)

  /**
    * SInt modulo
    */
  def Modulo[T <: SNumericType](left: Value[T], right: Value[T]): TwoArgumentsOperation[T,T,T] =
    DefaultSigmaBuilder.Modulo(left, right)


}
