package sigmastate.lang

import sigmastate.Values.SValue

class SigmaSpecializer {
  import SigmaSpecializer._

  def specialize(typed: SValue): SValue = {
    typed
  }


}

class SpecializerException(msg: String) extends Exception(msg)

object SigmaSpecializer {
  def error(msg: String) = throw new SpecializerException(msg)
}
