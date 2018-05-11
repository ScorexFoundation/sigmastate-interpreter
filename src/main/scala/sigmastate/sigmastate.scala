import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.{MinusCode, MultiplyCode, PlusCode}

package object sigmastate {

  /**
    * SInt addition
    */
  def Plus(left: Value[SInt.type], right: Value[SInt.type]): ArithmeticOperations = ArithmeticOperations(left, right, PlusCode)

  /**
    * SInt subtraction
    */
  def Minus(left: Value[SInt.type], right: Value[SInt.type]): ArithmeticOperations = ArithmeticOperations(left, right, MinusCode)

  /**
    * SInt multiplication
    */
  def Multiply(left: Value[SInt.type], right: Value[SInt.type]): ArithmeticOperations = ArithmeticOperations(left, right, MultiplyCode)

}
