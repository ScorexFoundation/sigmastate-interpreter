package sigmastate.lang

import sigmastate.Values.SValue

class SigmaSpecializer {
  import SigmaSpecializer._

  def specialize(typed: SValue): SValue = {
    typed
  }

  def error(msg: String) = throw new SpecializerException(msg)
}

class SpecializerException(msg: String) extends Exception(msg)

object SigmaSpecializer {

}
