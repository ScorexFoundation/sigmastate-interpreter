import sigmastate.Values.Value
import sigmastate.serialization.OpCodes._

package object sigmastate {

  /**
    * SInt addition
    */
  def Plus(left: Value[SInt.type], right: Value[SInt.type]): ArithOp = ArithOp(left, right, PlusCode)

  /**
    * SInt subtraction
    */
  def Minus(left: Value[SInt.type], right: Value[SInt.type]): ArithOp = ArithOp(left, right, MinusCode)

  /**
    * SInt multiplication
    */
  def Multiply(left: Value[SInt.type], right: Value[SInt.type]): ArithOp = ArithOp(left, right, MultiplyCode)

  /**
    * SInt division
    */
  def Divide(left: Value[SInt.type], right: Value[SInt.type]): ArithOp = ArithOp(left, right, DivisionCode)

  /**
    * SInt modulo
    */
  def Modulo(left: Value[SInt.type], right: Value[SInt.type]): ArithOp = ArithOp(left, right, ModuloCode)


}
