package sigmastate.eval

import org.ergoplatform.{ErgoLikeContext, ErgoBox}
import scalan.SigmaLibrary
import sigmastate._
import sigmastate.Values._
import sigmastate.SType.AnyOps
import sigmastate.interpreter.CryptoConstants

trait CostingRules extends SigmaLibrary { IR: RuntimeCosting =>
  import Coll._
  import BigInt._
  import SigmaProp._
  import AvlTree._
  import GroupElement._
  import CollBuilder._
  import SizeBuilder._
  import CostedBuilder._
  import Costed._
  import Size._
  import SizePrim._
  import SizeColl._
  import SizeOption._
  import SizePair._
  import SizeBox._
  import SizeContext._
  import CCostedPrim._
  import CCostedPair._
  import CCostedOption._
  import CCostedFunc._
  import CostedColl._
  import CCostedColl._
  import SigmaDslBuilder._
  import CostModel._
  import WSpecialPredef._
  import WRType._
  import WOption._
  import Box._

  abstract class CostingHandler[T](createCoster: (RCosted[T], SMethod, Seq[RCosted[_]]) => Coster[T]) {
    def apply(obj: RCosted[_], method: SMethod, args: Seq[RCosted[_]]): RCosted[_] = {
      val coster = createCoster(asCosted[T](obj), method, args)
      val costerClass = coster.getClass
      val costerMethod = costerClass.getMethod(method.name, Array.fill(args.length)(classOf[Sym]):_*)
      val res = costerMethod.invoke(coster, args:_*)
      res.asInstanceOf[RCosted[_]]
    }
  }

  /** Special graph node to represent accumulation of the operation costs.
    * In general, due to node sharing it is incorrect to just sum up all the `args` costs
    * and add `resCost` to that value.
    * Example: <br>
    * <code>
    * val x = ..
    * val y = op1(x)
    * val z = op2(x)
    * val res = op3(y, z)
    * </code>
    * The naive summation will lead to the cost of x` is accumulated both into `cost of y`
    * and into `cost of z`, so in the `cost of res` it is accumulated twice.
    * To avoid this problem OpCost nodes require special handling in during evaluation.
    *
    * @param  args    costs of the arguments, which are here represent dependency information.
    * @param  opCost operation cost, which should be added to the currently accumulated cost
    * @see `Evaluation`
    */
  case class OpCost(args: Seq[Rep[Int]], opCost: Rep[Int]) extends BaseDef[Int] {
    override def transform(t: Transformer) = OpCost(t(args), t(opCost))
  }
  def opCost(args: Seq[Rep[Int]], opCost: Rep[Int]): Rep[Int] = OpCost(args, opCost)

  def selectFieldCost = sigmaDslBuilder.CostModel.SelectField

  // TODO move initialization to init() to support resetContext
  lazy val SizeUnit: RSize[Unit] = costedBuilder.mkSizePrim(0L, UnitElement)
  lazy val SizeBoolean: RSize[Boolean] = costedBuilder.mkSizePrim(1L, BooleanElement)
  lazy val SizeByte: RSize[Byte] = costedBuilder.mkSizePrim(1L, ByteElement)
  lazy val SizeShort: RSize[Short] = costedBuilder.mkSizePrim(2L, ShortElement)
  lazy val SizeInt: RSize[Int] = costedBuilder.mkSizePrim(4L, IntElement)
  lazy val SizeLong: RSize[Long] = costedBuilder.mkSizePrim(8L, LongElement)
  lazy val SizeBigInt: RSize[BigInt] = costedBuilder.mkSizePrim(SBigInt.MaxSizeInBytes, element[BigInt])
  lazy val SizeString: RSize[String] = costedBuilder.mkSizePrim(256L, StringElement)
  lazy val SizeAvlTree: RSize[AvlTree] = costedBuilder.mkSizePrim(AvlTreeData.TreeDataSize.toLong, element[AvlTree])
  lazy val SizeGroupElement: RSize[GroupElement] = costedBuilder.mkSizePrim(CryptoConstants.EncodedGroupElementLength.toLong, element[GroupElement])

  lazy val SizeHashBytes: RSize[Coll[Byte]] = {
    val len: Rep[Int] = CryptoConstants.hashLength
    val sizes = colBuilder.replicate(len, SizeByte)
    costedBuilder.mkSizeColl(sizes)
  }

  def mkSizeSigmaProp(size: Rep[Long]): RSize[SigmaProp] = costedBuilder.mkSizePrim(size, element[SigmaProp])

  def SizeOfSigmaBoolean(sb: SigmaBoolean): RSize[SigmaProp] = mkSizeSigmaProp(SSigmaProp.dataSize(sb.asWrappedType))

  case class Cast[To](eTo: Elem[To], x: Rep[Def[_]]) extends BaseDef[To]()(eTo) {
    override def transform(t: Transformer) = Cast(eTo, t(x))
  }

  def tryCast[To](x: Rep[Def[_]])(implicit eTo: Elem[To]): Rep[To] = {
    if (eTo.runtimeClass.isAssignableFrom(x.elem.runtimeClass))
      x.asRep[To]
    else
      Cast(eTo, x)
  }

  def asCostedColl[T](collC: RCosted[Coll[T]]): Rep[CostedColl[T]] = {
    implicit val eT = collC.elem.eVal.eItem
    tryCast[CostedColl[T]](collC)
  }
  def asSizeColl[T](collS: RSize[Coll[T]]): Rep[SizeColl[T]] = {
    implicit val eT = collS.elem.eVal.eItem
    tryCast[SizeColl[T]](collS)
  }
  def asSizePair[A, B](s: RSize[(A,B)]): Rep[SizePair[A,B]] = {
    implicit val eA = s.elem.eVal.eFst
    implicit val eB = s.elem.eVal.eSnd
    tryCast[SizePair[A,B]](s)
  }
  def asSizeOption[T](optS: RSize[WOption[T]]): Rep[SizeOption[T]] = {
    implicit val eA = optS.elem.eVal.eItem
    tryCast[SizeOption[T]](optS)
  }
  def asSizeBox(ctx: RSize[Box]): Rep[SizeBox] = tryCast[SizeBox](ctx)
  def asSizeContext(ctx: RSize[Context]): Rep[SizeContext] = tryCast[SizeContext](ctx)

  def SOME[A](x: Rep[A]): Rep[WOption[A]] = RWSpecialPredef.some(x)

  def mkSizeColl[T:Elem](len: Rep[Int]): Rep[Size[Coll[T]]] = {
    val sizes = colBuilder.replicate(len, costedBuilder.mkSizePrim(typeSize[T], element[T]): RSize[T])
    costedBuilder.mkSizeColl(sizes)
  }

  def mkSizeColl[T](len: Rep[Int], sItem: RSize[T]): Rep[Size[Coll[T]]] = {
    val sizes = colBuilder.replicate(len, sItem)
    costedBuilder.mkSizeColl(sizes)
  }

  def mkSizeOption[T](size: RSize[T]): Rep[Size[WOption[T]]] = costedBuilder.mkSizeOption(SOME(size))
  def mkSizePair[A, B](l: RSize[A], r: RSize[B]): Rep[Size[(A,B)]] = costedBuilder.mkSizePair(l, r)

  def mkCostedColl[T](values: RColl[T], costs: RColl[Int], sizes: RColl[Size[T]], valuesCost: Rep[Int]): RCostedColl[T] =
    costedBuilder.mkCostedColl(values, costs, sizes, valuesCost)

  def mkCostedOption[T](value: ROption[T], costOpt: ROption[Int], sizeOpt: ROption[Size[T]], accCost: Rep[Int]): RCostedOption[T] =
    costedBuilder.mkCostedOption(value, costOpt, sizeOpt, accCost)

  def mkCostedFunc[A,R](f: RFuncCosted[A,R], cost: Rep[Int], codeSize: Rep[Long], eArg: Elem[A], eRes: Elem[R]): Rep[CostedFunc[Unit, A, R]] = {
    val envC = RCCostedPrim((), 0, SizeUnit)
    val sFunc = costedBuilder.mkSizeFunc(SizeUnit, codeSize, eArg, eRes)
    RCCostedFunc(envC, f, cost, sFunc)
  }

  abstract class Coster[T](obj: RCosted[T], method: SMethod, args: Seq[RCosted[_]]) {
    def costOfArgs = (obj +: args).map(_.cost)
    def sizeOfArgs = args.foldLeft(obj.size.dataSize)({ case (s, e) => s + e.size.dataSize })

    def constantSizeProperyAccess[R](prop: Rep[T] => Rep[R]): RCosted[R] =
      withConstantSize(prop(obj.value), opCost(costOfArgs, selectFieldCost))

    def knownSizeProperyAccess[R](prop: Rep[T] => Rep[R], size: RSize[R]): RCosted[R] =
      RCCostedPrim(prop(obj.value), opCost(costOfArgs, selectFieldCost), size)

    def knownLengthCollProperyAccess[R](prop: Rep[T] => Rep[Coll[R]], len: Rep[Int]): Rep[CostedColl[R]] =
      mkCostedColl(prop(obj.value), len, opCost(costOfArgs, selectFieldCost))

    def digest32ProperyAccess(prop: Rep[T] => Rep[Coll[Byte]]): Rep[CostedColl[Byte]] =
      knownLengthCollProperyAccess(prop, CryptoConstants.hashLength)

    def groupElementProperyAccess(prop: Rep[T] => Rep[GroupElement]): RCosted[GroupElement] =
      knownSizeProperyAccess(prop, SizeGroupElement)

    def bigIntProperyAccess(prop: Rep[T] => Rep[BigInt]): RCosted[BigInt] =
      knownSizeProperyAccess(prop, SizeBigInt)

    def defaultProperyAccess[R](prop: Rep[T] => Rep[R], propSize: RSize[T] => RSize[R]): RCosted[R] =
      RCCostedPrim(prop(obj.value), opCost(costOfArgs, selectFieldCost), propSize(obj.size))

    def defaultOptionProperyAccess[R: Elem](prop: Rep[T] => ROption[R], propSize: RSize[T] => RSize[WOption[R]], itemCost: Rep[Int]): RCostedOption[R] = {
      val v = prop(obj.value)
      val s = propSize(obj.size)
      RCCostedOption(v, SOME(itemCost), asSizeOption(s).sizeOpt, opCost(costOfArgs, selectFieldCost))
    }

    def defaultCollProperyAccess[R: Elem](prop: Rep[T] => RColl[R], propSize: RSize[T] => RSize[Coll[R]], itemCost: Rep[Int]): RCostedColl[R] = {
      val v = prop(obj.value)
      val s = propSize(obj.size)
      val sizes = asSizeColl(s).sizes
      val costs = colBuilder.replicate(sizes.length, itemCost)
      RCCostedColl(v, costs, sizes, opCost(costOfArgs, selectFieldCost))
    }

    def boxPropertyAccess(prop: Rep[T] => Rep[Box], propSize: RSize[T] => RSize[Box]): RCosted[Box] = {
      val v = prop(obj.value)
      val c = opCost(costOfArgs, sigmaDslBuilder.CostModel.AccessBox)
      val s = propSize(obj.size)
      RCCostedPrim(v, c, s)
    }

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
    def digest() = knownLengthCollProperyAccess(_.digest, AvlTreeData.DigestSize)
    def enabledOperations() = constantSizeProperyAccess(_.enabledOperations)
    def keyLength() = constantSizeProperyAccess(_.keyLength)
    def valueLengthOpt() = defaultOptionProperyAccess(_.valueLengthOpt, _ => mkSizeOption(SizeInt), 0)
    def isInsertAllowed() = constantSizeProperyAccess(_.isInsertAllowed)
    def isUpdateAllowed() = constantSizeProperyAccess(_.isUpdateAllowed)
    def isRemoveAllowed() = constantSizeProperyAccess(_.isRemoveAllowed)

    def updateOperations(flags: RCosted[Byte]) = {
      RCCostedPrim(obj.value.updateOperations(flags.value), opCost(costOfArgs, costOf(method)), obj.size)
    }
    def contains(key: RCosted[Coll[Byte]], proof: RCosted[Coll[Byte]]): RCosted[Boolean] = {
      withConstantSize(obj.value.contains(key.value, proof.value), opCost(costOfArgs, perKbCostOf(method, sizeOfArgs)))
    }
    def get(key: RCosted[Coll[Byte]], proof: RCosted[Coll[Byte]]): RCosted[WOption[Coll[Byte]]] = {
      val value = obj.value.get(key.value, proof.value)
      val size = sizeOfArgs
      val c = opCost(costOfArgs, perKbCostOf(method, size))
      val res = RCCostedOption(value,
        RWSpecialPredef.some(0),
        RWSpecialPredef.some(proof.size),
        c)
      res
    }
    def getMany(_keys: RCosted[Coll[Coll[Byte]]], _proof: RCosted[Coll[Byte]]): RCosted[Coll[WOption[Coll[Byte]]]] = {
      val keysC = asCostedColl[Coll[Byte]](_keys)
      val proofC = asCostedColl[Byte](_proof)
      val nKeys = keysC.sizes.length

      val value = obj.value.getMany(keysC.value, proofC.value)
      val costs = colBuilder.replicate(nKeys, 0)
      val valuesCost = opCost(costOfArgs, perKbCostOf(method, sizeOfArgs))

      val treeValueLengthPerKey = proofC.sizes.length div nKeys
      val treeValueS = mkSizeColl[Byte](treeValueLengthPerKey)
      val sizes = colBuilder.replicate(nKeys, mkSizeOption(treeValueS))

      val res = RCCostedColl(value, costs, sizes, valuesCost)
      res
    }

    private def treeModifierMethod(meth: Rep[AvlTree] => Rep[WOption[AvlTree]]): RCosted[WOption[AvlTree]] = {
      val value = meth(obj.value)
      val size = sizeOfArgs
      RCCostedOption(value,
        RWSpecialPredef.some(0),
        SOME(obj.size), opCost(costOfArgs, perKbCostOf(method, size)))
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
    def boxCollProperty(prop: Rep[Context] => Rep[Coll[Box]], propSize: Rep[SizeContext] => RSize[Coll[Box]]) = {
      defaultCollProperyAccess(prop, ctxS => propSize(asSizeContext(ctxS)), sigmaDslBuilder.CostModel.AccessBox)
    }
    def headers() = {
      knownLengthCollProperyAccess(_.headers, ErgoLikeContext.MaxHeaders)
    }
    def preHeader() = constantSizeProperyAccess(_.preHeader)

    def dataInputs(): RCostedColl[Box] = {
      boxCollProperty(_.dataInputs, _.dataInputs)
    }

    def OUTPUTS: RCostedColl[Box] = {
      boxCollProperty(_.OUTPUTS, _.outputs)
    }

    def INPUTS: RCostedColl[Box] = {
      boxCollProperty(_.INPUTS, _.inputs)
    }

    def HEIGHT: RCosted[Int] = constantSizeProperyAccess(_.HEIGHT)

    def SELF: RCosted[Box] = boxPropertyAccess(_.SELF, asSizeContext(_).selfBox)

    def LastBlockUtxoRootHash: RCosted[AvlTree] =
      knownSizeProperyAccess(_.LastBlockUtxoRootHash, SizeAvlTree)

    def minerPubKey: RCostedColl[Byte] =
      knownLengthCollProperyAccess(_.minerPubKey, CryptoConstants.EncodedGroupElementLength.toInt)

    def getVar[T](id: RCosted[Byte])(implicit tT: Rep[WRType[T]]): RCostedOption[T] = { ???
//      defaultOptionProperyAccess(_.getVar(id.value)(tT.eA), asSizeContext(_).reg)
//      val opt = ctx.getVar(id)(cT)
//      dsl.costOption(opt, dsl.CostModel.GetVar)
    }

    def selfBoxIndex: RCosted[Int] = constantSizeProperyAccess(_.selfBoxIndex)

  }

  object ContextCoster extends CostingHandler[Context]((obj, m, args) => new ContextCoster(obj, m, args))

  class BoxCoster(obj: RCosted[Box], method: SMethod, args: Seq[RCosted[_]]) extends Coster[Box](obj, method, args){
    import Box._
    import ErgoBox._

    def creationInfo: RCosted[(Int, Coll[Byte])] = {
      val info = obj.value.creationInfo
      val cost = opCost(Seq(obj.cost), sigmaDslBuilder.CostModel.SelectField)
      val l = RCCostedPrim(info._1, cost, SizeInt)
      val r = mkCostedColl(info._2, CryptoConstants.hashLength, cost)
      RCCostedPair(l, r)
    }

    def tokens() = {
      val tokens = obj.value.tokens
      val sTokens = asSizeColl(asSizeBox(obj.size).tokens).sizes
      val sTokenId = SizeHashBytes
      val sToken = mkSizePair(sTokenId, SizeLong)
      val len = sTokens.length
      val sInfo = mkSizeColl(len, sToken)
      val costs = colBuilder.replicate(len, 0)
      val sizes = colBuilder.replicate(len, sToken)
      RCCostedColl(tokens, costs, sizes, opCost(Seq(obj.cost), costOf(method)))
    }

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

    def version() = constantSizeProperyAccess(_.version)

    def parentId() = digest32ProperyAccess(_.parentId)

    def ADProofsRoot() = digest32ProperyAccess(_.ADProofsRoot)

    def stateRoot() = knownSizeProperyAccess(_.stateRoot, SizeAvlTree)

    def transactionsRoot() = digest32ProperyAccess(_.transactionsRoot)

    def timestamp() = constantSizeProperyAccess(_.timestamp)

    def nBits() = constantSizeProperyAccess(_.nBits)

    def height() = constantSizeProperyAccess(_.height)

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

    def version() = constantSizeProperyAccess(_.version)

    def parentId() = digest32ProperyAccess(_.parentId)

    def timestamp() = constantSizeProperyAccess(_.timestamp)

    def nBits() = constantSizeProperyAccess(_.nBits)

    def height() = constantSizeProperyAccess(_.height)

    def minerPk() = groupElementProperyAccess(_.minerPk)

    def votes() = knownLengthCollProperyAccess(_.votes, 3)
  }

  object PreHeaderCoster extends CostingHandler[PreHeader]((obj, m, args) => new PreHeaderCoster(obj, m, args))

  class OptionCoster[T](obj: RCosted[WOption[T]], method: SMethod, args: Seq[RCosted[_]]) extends Coster[WOption[T]](obj, method, args){
    import WOption._
    implicit val eT = obj.elem.eVal.eItem
    def get(): RCosted[T] = defaultProperyAccess(_.get, asSizeOption(_).sizeOpt.get)
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
