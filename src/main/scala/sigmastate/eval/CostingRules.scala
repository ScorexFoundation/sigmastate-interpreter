package sigmastate.eval

import scalan.SigmaLibrary
import sigmastate.SMethod

trait CostingRules extends SigmaLibrary { IR: RuntimeCosting =>
  import Costed._
  import CCostedPrim._

  trait CostRule {
    def apply(obj: RCosted[_], method: SMethod, args: Seq[RCosted[_]], cost: Rep[Int]): RCosted[_]
  }

  class AvlTreeCoster(obj: RCosted[AvlTree], method: SMethod, cost: Rep[Int]) {
    import AvlTree._
    def digest() = {
      mkCostedColl(obj.value.digest, cost)
    }
    def enableOperations(obj: RCosted[AvlTree]) = {
      withDefaultSize(obj.value.enabledOperations, cost + selectFieldCost)
    }
    def updateOperations(flags: RCosted[Byte]) = {
      RCCostedPrim(obj.value.updateOperations(flags.value), cost + costOf(method), obj.dataSize)
    }
    def contains(key: RCosted[Coll[Byte]], proof: RCosted[Coll[Byte]]): RCosted[Boolean] = {
      val inputSize = obj.dataSize + key.dataSize + proof.dataSize
      withDefaultSize(obj.value.contains(key.value, proof.value), cost + perKbCostOf(method, inputSize))
    }
  }

  object AvlTreeCoster extends CostRule {
    override def apply(obj: RCosted[_], method: SMethod, args: Seq[RCosted[_]], cost: Rep[Int]): RCosted[_] = {
      val coster = new AvlTreeCoster(asCosted[AvlTree](obj), method, cost)
      val costerClass = classOf[AvlTreeCoster]
      val costerMethod = costerClass.getMethod(method.name, Array.fill(args.length)(classOf[Sym]):_*)
      val res = costerMethod.invoke(coster, args:_*)
      res.asInstanceOf[RCosted[_]]
    }
  }
}
