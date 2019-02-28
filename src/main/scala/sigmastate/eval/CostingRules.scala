package sigmastate.eval

import scalan.SigmaLibrary
import sigmastate.SMethod

trait CostingRules extends SigmaLibrary { IR: RuntimeCosting =>
  import Coll._
  import CollBuilder._
  import Costed._
  import CCostedPrim._
  import CCostedOption._
  import CCostedColl._
  import SigmaDslBuilder._
  import CostModel._
  import WSpecialPredef._

  trait CostRule {
    def apply(obj: RCosted[_], method: SMethod, args: Seq[RCosted[_]]): RCosted[_]
  }

  def selectFieldCost = sigmaDslBuilder.CostModel.SelectField

  abstract class Coster[T](obj: RCosted[T], method: SMethod, args: Seq[RCosted[_]]) {
    def costOfArgs = args.foldLeft(obj.cost)({ case (s, e) => s + e.cost })
    def sizeOfArgs = args.foldLeft(obj.dataSize)({ case (s, e) => s + e.dataSize })

    def defaultProperyAccess[R](prop: Rep[T] => Rep[R]): RCosted[R] =
      withDefaultSize(prop(obj.value), costOfArgs + selectFieldCost)
    def defaultCollProperyAccess[R](prop: Rep[T] => Rep[Coll[R]]): Rep[CostedColl[R]] =
      mkCostedColl(prop(obj.value), costOfArgs + selectFieldCost)
    def defaultOptionProperyAccess[R](prop: Rep[T] => Rep[WOption[R]]): Rep[CostedOption[R]] = {
      val v = prop(obj.value)
      RCCostedOption(v, RWSpecialPredef.some(0), RWSpecialPredef.some(obj.dataSize), costOfArgs + selectFieldCost)
    }
  }

  class AvlTreeCoster(obj: RCosted[AvlTree], method: SMethod, args: Seq[RCosted[_]]) extends Coster[AvlTree](obj, method, args){
    import AvlTree._
    def digest() = defaultCollProperyAccess(_.digest)
    def enabledOperations() = defaultProperyAccess(_.enabledOperations)
    def keyLength() = defaultProperyAccess(_.keyLength)
    def valueLengthOpt() = defaultOptionProperyAccess(_.valueLengthOpt)
    def isInsertAllowed() = defaultProperyAccess(_.isInsertAllowed)
    def isUpdateAllowed() = defaultProperyAccess(_.isUpdateAllowed)
    def isRemoveAllowed() = defaultProperyAccess(_.isRemoveAllowed)

    def updateOperations(flags: RCosted[Byte]) = {
      RCCostedPrim(obj.value.updateOperations(flags.value), costOfArgs + costOf(method), obj.dataSize)
    }
    def contains(key: RCosted[Coll[Byte]], proof: RCosted[Coll[Byte]]): RCosted[Boolean] = {
      withDefaultSize(obj.value.contains(key.value, proof.value), costOfArgs + perKbCostOf(method, sizeOfArgs))
    }
    def get(key: RCosted[Coll[Byte]], proof: RCosted[Coll[Byte]]): RCosted[WOption[Coll[Byte]]] = {
      val value = obj.value.get(key.value, proof.value)
      val size = sizeOfArgs
      RCCostedOption(value,
        RWSpecialPredef.some(perKbCostOf(method, size)),
        RWSpecialPredef.some(size),
        costOfArgs)
    }
    def getMany(keysC: RCosted[Coll[Coll[Byte]]], proof: RCosted[Coll[Byte]]): RCosted[Coll[WOption[Coll[Byte]]]] = {
      val keys = keysC.value
      val value = obj.value.getMany(keys, proof.value)
      val len = keys.length
      val costs = colBuilder.replicate(len, 0)
      val inputSize = sizeOfArgs
      val sizes = colBuilder.replicate(len, inputSize div len.toLong)
      RCCostedColl(value, costs, sizes, costOfArgs + perKbCostOf(method, inputSize))
    }
    private def treeModifierMethod[R](meth: Rep[AvlTree] => Rep[WOption[R]]): RCosted[WOption[R]] = {
      val value = meth(obj.value)
      val size = sizeOfArgs
      RCCostedOption(value,
        RWSpecialPredef.some(perKbCostOf(method, size)),
        RWSpecialPredef.some(size), costOfArgs)
    }

    def insert(kvs: RCosted[Coll[(Coll[Byte], Coll[Byte])]], proof: RCosted[Coll[Byte]]): RCosted[WOption[AvlTree]] = {
      treeModifierMethod(_.insert(kvs.value, proof.value))
    }
    def update(kvs: RCosted[Coll[(Coll[Byte], Coll[Byte])]], proof: RCosted[Coll[Byte]]): RCosted[WOption[AvlTree]] = {
      treeModifierMethod(_.update(kvs.value, proof.value))
    }
    def remove(keys: RCosted[Coll[Coll[Byte]]], proof: RCosted[Coll[Byte]]): RCosted[WOption[AvlTree]] = {
      treeModifierMethod(_.remove(keys.value, proof.value))
    }
  }

  object AvlTreeCoster extends CostRule {
    override def apply(obj: RCosted[_], method: SMethod, args: Seq[RCosted[_]]): RCosted[_] = {
      val coster = new AvlTreeCoster(asCosted[AvlTree](obj), method, args)
      val costerClass = classOf[AvlTreeCoster]
      val costerMethod = costerClass.getMethod(method.name, Array.fill(args.length)(classOf[Sym]):_*)
      val res = costerMethod.invoke(coster, args:_*)
      res.asInstanceOf[RCosted[_]]
    }
  }
}
