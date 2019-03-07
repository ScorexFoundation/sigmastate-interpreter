package sigmastate.eval

import org.ergoplatform.{ErgoBox, ErgoLikeContext}
import scalan.SigmaLibrary
import sigmastate.{AvlTreeData, SBigInt, SLong, SMethod}
import sigmastate.SType.AnyOps
import sigmastate.interpreter.CryptoConstants

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

  abstract class CostingHandler[T](createCoster: (RCosted[T], SMethod, Seq[RCosted[_]]) => Coster[T]) {
    def apply(obj: RCosted[_], method: SMethod, args: Seq[RCosted[_]]): RCosted[_] = {
      val coster = createCoster(asCosted[T](obj), method, args)
      val costerClass = coster.getClass
      val costerMethod = costerClass.getMethod(method.name, Array.fill(args.length)(classOf[Sym]):_*)
      val res = costerMethod.invoke(coster, args:_*)
      res.asInstanceOf[RCosted[_]]
    }
  }

  def selectFieldCost = sigmaDslBuilder.CostModel.SelectField

  abstract class Coster[T](obj: RCosted[T], method: SMethod, args: Seq[RCosted[_]]) {
    def costOfArgs = args.foldLeft(obj.cost)({ case (s, e) => s + e.cost })
    def sizeOfArgs = args.foldLeft(obj.dataSize)({ case (s, e) => s + e.dataSize })

    def defaultProperyAccess[R](prop: Rep[T] => Rep[R]): RCosted[R] =
      withDefaultSize(prop(obj.value), costOfArgs + selectFieldCost)

    def knownSizeProperyAccess[R](prop: Rep[T] => Rep[R], size: Rep[Long]): RCosted[R] =
      RCCostedPrim(prop(obj.value), costOfArgs + selectFieldCost, size)

    def defaultCollProperyAccess[R](prop: Rep[T] => Rep[Coll[R]]): Rep[CostedColl[R]] =
      mkCostedColl(prop(obj.value), costOfArgs + selectFieldCost)

    def knownLengthCollProperyAccess[R](prop: Rep[T] => Rep[Coll[R]], len: Rep[Int]): Rep[CostedColl[R]] =
      mkCostedColl(prop(obj.value), len, costOfArgs + selectFieldCost)

    def digest32ProperyAccess(prop: Rep[T] => Rep[Coll[Byte]]): Rep[CostedColl[Byte]] =
      knownLengthCollProperyAccess(prop, CryptoConstants.hashLength)

    def groupElementProperyAccess(prop: Rep[T] => Rep[GroupElement]): RCosted[GroupElement] =
      knownSizeProperyAccess(prop, CryptoConstants.EncodedGroupElementLength.toLong)

    def bigIntProperyAccess(prop: Rep[T] => Rep[BigInt]): RCosted[BigInt] =
      knownSizeProperyAccess(prop, SBigInt.MaxSizeInBytes)

    def defaultOptionProperyAccess[R](prop: Rep[T] => Rep[WOption[R]]): Rep[CostedOption[R]] = {
      val v = prop(obj.value)
      RCCostedOption(v, RWSpecialPredef.some(0), RWSpecialPredef.some(obj.dataSize), costOfArgs + selectFieldCost)
    }

//    def costBoxes(bs: Coll[Box]): CostedColl[Box] = {
//      val len = bs.length
//      val perItemCost = this.CostModel.AccessBox
//      val costs = this.Colls.replicate(len, perItemCost)
//      val sizes = bs.map(b => b.dataSize)
//      val valuesCost = this.CostModel.CollectionConst
//      this.Costing.mkCostedColl(bs, costs, sizes, valuesCost)
//    }
//
//    /** Cost of collection with static size elements. */
//    def costColWithConstSizedItem[T](xs: Coll[T], len: Int, itemSize: Long): CostedColl[T] = {
//      val perItemCost = (len.toLong * itemSize / 1024L + 1L) * this.CostModel.AccessKiloByteOfData.toLong
//      val costs = this.Colls.replicate(len, perItemCost.toInt)
//      val sizes = this.Colls.replicate(len, itemSize)
//      val valueCost = this.CostModel.CollectionConst
//      this.Costing.mkCostedColl(xs, costs, sizes, valueCost)
//    }
//
//    def costOption[T](opt: Option[T], opCost: Int)(implicit cT: RType[T]): CostedOption[T] = {
//      val none = this.Costing.mkCostedNone[T](opCost)
//      opt.fold[CostedOption[T]](none)(x => this.Costing.mkCostedSome(this.Costing.costedValue(x, SpecialPredef.some(opCost))))
//    }
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
      val res = RCCostedOption(value,
        RWSpecialPredef.some(perKbCostOf(method, size)),
        RWSpecialPredef.some(sizeData(element[Coll[Byte]], colBuilder.replicate(proof.dataSize.toInt, 1L))),
        costOfArgs)
      res
    }
    def getMany(keysC: RCosted[Coll[Coll[Byte]]], proof: RCosted[Coll[Byte]]): RCosted[Coll[WOption[Coll[Byte]]]] = {
      val keys = keysC.value
      val value = obj.value.getMany(keys, proof.value)
      val len = keys.length
      val costs = colBuilder.replicate(len, 0)
      val inputSize = sizeOfArgs
      val sizes = colBuilder.replicate(len, inputSize div len.toLong)
      val res = RCCostedColl(value, costs, sizes, costOfArgs + perKbCostOf(method, inputSize))
      res
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

  object AvlTreeCoster extends CostingHandler[AvlTree]((obj, m, args) => new AvlTreeCoster(obj, m, args))

  class ContextCoster(obj: RCosted[Context], method: SMethod, args: Seq[RCosted[_]]) extends Coster[Context](obj, method, args){
    import Context._
    def dataInputs() = {
      sigmaDslBuilder.costBoxes(obj.value.dataInputs)
    }
    def headers() = {
      knownLengthCollProperyAccess(_.headers, ErgoLikeContext.MaxHeaders)
    }
    def preHeader() = defaultProperyAccess(_.preHeader)

//    def OUTPUTS: CostedColl[Box] = dsl.costBoxes(ctx.OUTPUTS)
//    def INPUTS: CostedColl[Box] = dsl.costBoxes(ctx.INPUTS)
//    def HEIGHT: Costed[Int] = {
//      val cost = dsl.CostModel.SelectField
//      new CCostedPrim(ctx.HEIGHT, cost, 4L)
//    }
//    def SELF: CostedBox = new CCostedBox(ctx.SELF, dsl.CostModel.AccessBox)
//    def LastBlockUtxoRootHash: Costed[AvlTree] = {
//      val tree = ctx.LastBlockUtxoRootHash
//      new CCostedPrim(tree, dsl.CostModel.AccessAvlTree, tree.dataSize)
//    }
//    def minerPubKey: CostedColl[Byte] = dsl.costColWithConstSizedItem(ctx.minerPubKey, dsl.CostModel.PubKeySize.toInt, 1)
//    def getVar[T](id: Byte)(implicit cT: RType[T]): CostedOption[T] = {
//      val opt = ctx.getVar(id)(cT)
//      dsl.costOption(opt, dsl.CostModel.GetVar)
//    }
//
//    def value = ctx
//    def cost = ctx.cost
//    def dataSize = ctx.dataSize
//
//    def selfBoxIndex: Costed[Int] = {
//      val cost = dsl.CostModel.SelectField
//      new CCostedPrim(ctx.selfBoxIndex, cost, 4L)
//    }
//    def cost = (dataSize / builder.CostModel.AccessKiloByteOfData.toLong).toInt
//
//    def dataSize = {
//      val inputsSize = INPUTS.map(_.dataSize).sum(builder.Monoids.longPlusMonoid)
//      val outputsSize = OUTPUTS.map(_.dataSize).sum(builder.Monoids.longPlusMonoid)
//      8L + (if (SELF == null) 0 else SELF.dataSize) + inputsSize + outputsSize + LastBlockUtxoRootHash.dataSize
//    }

  }

  object ContextCoster extends CostingHandler[Context]((obj, m, args) => new ContextCoster(obj, m, args))

  class BoxCoster(obj: RCosted[Box], method: SMethod, args: Seq[RCosted[_]]) extends Coster[Box](obj, method, args){
    import Box._
    import ErgoBox._
    def tokens() = {
      val len = MaxTokens.toInt
      val tokens = obj.value.tokens
      val tokenInfoSize = sizeData(tokens.elem.eItem, Pair(TokenId.size.toLong, SLong.dataSize(0L.asWrappedType)))
      val costs = colBuilder.replicate(len, 0)
      val sizes = colBuilder.replicate(len, tokenInfoSize)
      RCCostedColl(tokens, costs, sizes, obj.cost + costOf(method))
    }
    //  @NeverInline
    //  def cost = (dataSize / builder.CostModel.AccessKiloByteOfData.toLong).toInt
    //  @NeverInline
    //  def dataSize = bytes.length

//    def id: CostedColl[Byte] = dsl.costColWithConstSizedItem(box.id, box.id.length, 1)
//    def valueCosted: Costed[Long] = {
//      val cost = dsl.CostModel.SelectField
//      new CCostedPrim(box.value, cost, 8L)
//    }
//    def bytes: CostedColl[Byte] = dsl.costColWithConstSizedItem(box.bytes, box.bytes.length, 1)
//    def bytesWithoutRef: CostedColl[Byte] = dsl.costColWithConstSizedItem(box.bytesWithoutRef, box.bytesWithoutRef.length, 1)
//    def propositionBytes: CostedColl[Byte] = dsl.costColWithConstSizedItem(box.propositionBytes, box.propositionBytes.length, 1)
//    def registers: CostedColl[AnyValue] = {
//      val len = box.registers.length
//      val costs = dsl.Colls.replicate(len, dsl.CostModel.AccessBox)
//      val sizes = box.registers.map(o => o.dataSize)
//      new CCostedColl(box.registers, costs, sizes, dsl.CostModel.CollectionConst)
//    }
//    def getReg[@Reified T](id: Int)(implicit cT:RType[T]): CostedOption[T] = {
//      val opt = box.getReg(id)(cT)
//      dsl.costOption(opt, dsl.CostModel.GetRegister)
//    }
//
//    @NeverInline
//    def creationInfo: Costed[(Int, Coll[Byte])] = SpecialPredef.rewritableMethod
  }

  object BoxCoster extends CostingHandler[Box]((obj, m, args) => new BoxCoster(obj, m, args))

  class HeaderCoster(obj: RCosted[Header], method: SMethod, args: Seq[RCosted[_]]) extends Coster[Header](obj, method, args){
    import Header._

    def version() = defaultProperyAccess(_.version)

    def parentId() = digest32ProperyAccess(_.parentId)

    def ADProofsRoot() = digest32ProperyAccess(_.ADProofsRoot)

    def stateRoot() = knownSizeProperyAccess(_.stateRoot, AvlTreeData.TreeDataSize.toLong)

    def transactionsRoot() = digest32ProperyAccess(_.transactionsRoot)

    def timestamp() = defaultProperyAccess(_.timestamp)

    def nBits() = defaultProperyAccess(_.nBits)

    def height() = defaultProperyAccess(_.height)

    def extensionRoot() = digest32ProperyAccess(_.extensionRoot)

    def minerPk() = groupElementProperyAccess(_.minerPk)

    def powOnetimePk() = groupElementProperyAccess(_.powOnetimePk)

    def powNonce() = knownLengthCollProperyAccess(_.powNonce, 8)

    def powDistance() = bigIntProperyAccess(_.powDistance)

    def votes() = knownLengthCollProperyAccess(_.votes, 3)
  }

  object HeaderCoster extends CostingHandler[Header]((obj, m, args) => new HeaderCoster(obj, m, args))

  class PreHeaderCoster(obj: RCosted[PreHeader], method: SMethod, args: Seq[RCosted[_]]) extends Coster[PreHeader](obj, method, args){
    import PreHeader._

//    def id() = digest32ProperyAccess(_.id)

    def version() = defaultProperyAccess(_.version)

    def parentId() = digest32ProperyAccess(_.parentId)

    def timestamp() = defaultProperyAccess(_.timestamp)

    def nBits() = defaultProperyAccess(_.nBits)

    def height() = defaultProperyAccess(_.height)

    def minerPk() = groupElementProperyAccess(_.minerPk)

    def votes() = knownLengthCollProperyAccess(_.votes, 3)
  }

  object PreHeaderCoster extends CostingHandler[PreHeader]((obj, m, args) => new PreHeaderCoster(obj, m, args))

  class OptionCoster[T](obj: RCosted[WOption[T]], method: SMethod, args: Seq[RCosted[_]]) extends Coster[WOption[T]](obj, method, args){
    import WOption._
//    def get: Costed[T] = builder.mkCostedPrim(value.get, cost, dataSize)
//    def getOrElse(default: Costed[T]): Costed[T] = {
//      val v = value.getOrElse(default.value)
//      val c = accumulatedCost + costOpt.getOrElse(default.cost)
//      val s = sizeOpt.getOrElse(default.dataSize)
//      builder.mkCostedPrim(v, c, s)
//    }
//    def isEmpty: Costed[Boolean] = builder.mkCostedPrim(value.isEmpty, cost, 1L)
//    def isDefined: Costed[Boolean] = builder.mkCostedPrim(value.isDefined, cost, 1L)
  }

  object OptionCoster extends CostingHandler[WOption[Any]]((obj, m, args) => new OptionCoster[Any](obj, m, args))
}
