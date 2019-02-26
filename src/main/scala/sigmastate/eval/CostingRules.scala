package sigmastate.eval

import scalan.SigmaLibrary
import sigmastate.SMethod

trait CostingRules extends SigmaLibrary { IR: RuntimeCosting =>
  import Costed._
  import CCostedPrim._

  class AvlTreeCosting(obj: RCosted[AvlTree], method: SMethod, cost: Rep[Int]) {
    import AvlTree._
    def digest() = {
      mkCostedColl(obj.value.digest, cost)
    }
    def enableOperations(obj: RCosted[AvlTree], cost: Rep[Int]) = {
      withDefaultSize(obj.value.enabledOperations, cost + selectFieldCost)
    }
    def updateOperations(flags: RCosted[Byte]) = {
      RCCostedPrim(obj.value.updateOperations(flags.value), cost + costOf(method), obj.dataSize)
    }
  }
}
