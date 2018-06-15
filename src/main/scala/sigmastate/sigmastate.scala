import sigmastate.Values.Value
import sigmastate.serialization.OpCodes._

package object sigmastate {

  /**
    * SInt addition
    */
  def Plus[T <: SNumericType](left: Value[T], right: Value[T]): ArithOp[T] = ArithOp(left, right, PlusCode)

  /**
    * SInt subtraction
    */
  def Minus[T <: SNumericType](left: Value[T], right: Value[T]): ArithOp[T] = ArithOp(left, right, MinusCode)

  /**
    * SInt multiplication
    */
  def Multiply[T <: SNumericType](left: Value[T], right: Value[T]): ArithOp[T] = ArithOp(left, right, MultiplyCode)

  /**
    * SInt division
    */
  def Divide[T <: SNumericType](left: Value[T], right: Value[T]): ArithOp[T] = ArithOp(left, right, DivisionCode)

  /**
    * SInt modulo
    */
  def Modulo[T <: SNumericType](left: Value[T], right: Value[T]): ArithOp[T] = ArithOp(left, right, ModuloCode)


}
