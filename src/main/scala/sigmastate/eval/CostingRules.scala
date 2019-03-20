package sigmastate.eval

import org.ergoplatform.{ErgoLikeContext, ErgoBox}
import scalan.{SigmaLibrary, RType}
import sigmastate._
import sigmastate.Values._
import sigmastate.SType.AnyOps
import sigmastate.interpreter.CryptoConstants
import sigmastate.utxo.CostTable
import special.collection.Coll

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
  import CSizePair._
  import SizeBox._
  import SizeContext._
  import CCostedPrim._
  import CostedPair._
  import CCostedPair._
  import CCostedOption._
  import CostedFunc._
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
  def asCostedPair[A,B](pC: RCosted[(A,B)]): Rep[CostedPair[A,B]] = {
    implicit val eA = pC.elem.eVal.eFst
    implicit val eB = pC.elem.eVal.eSnd
    tryCast[CostedPair[A,B]](pC)
  }
  def asCostedFunc[A,B](fC: RCosted[A => B]): Rep[CostedFunc[Unit,A,B]] = {
    implicit val eA = fC.elem.eVal.eDom
    implicit val eB = fC.elem.eVal.eRange
    tryCast[CostedFunc[Unit, A, B]](fC)(costedFuncElement(UnitElement, eA, eB))
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

  class GroupElementCoster(obj: RCosted[GroupElement], method: SMethod, args: Seq[RCosted[_]]) extends Coster[GroupElement](obj, method, args){
    import GroupElement._
    def getEncoded: RCosted[Coll[Byte]] =
      knownLengthCollProperyAccess(_.getEncoded, CryptoConstants.EncodedGroupElementLength.toInt)

    def negate: RCosted[GroupElement] = {
      RCCostedPrim(obj.value.negate, opCost(costOfArgs, costOf(method)), SizeGroupElement)
    }
  }

  object GroupElementCoster extends CostingHandler[GroupElement]((obj, m, args) => new GroupElementCoster(obj, m, args))

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
      val cost = opCost(info, Seq(obj.cost), sigmaDslBuilder.CostModel.GetRegister)
      val l = RCCostedPrim(info._1, 0, SizeInt)
      val r = mkCostedColl(info._2, CryptoConstants.hashLength, 0)
      RCCostedPair(l, r, cost)
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

    def getReg[T](i: RCosted[Int])(implicit tT: Rep[WRType[T]]): RCosted[WOption[T]] = {
      val sBox = asSizeBox(obj.size)
      implicit val elem = tT.eA
      val valueOpt = obj.value.getReg(i.value)(elem)
      val sReg = asSizeOption(sBox.getReg(downcast[Byte](i.value))(elem))
      RCCostedOption(valueOpt, SOME(0), sReg.sizeOpt, opCost(valueOpt, Seq(obj.cost), sigmaDslBuilder.CostModel.GetRegister))
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

    def getOrElse(default: RCosted[T]): RCosted[T] = {
      val v = obj.value.getOrElse(default.value)
      val c = opCost(costOfArgs, selectFieldCost)
      val s = asSizeOption(obj.size).sizeOpt.getOrElse(default.size)
      RCCostedPrim(v, c, s)
    }

    def isDefined: RCosted[Boolean] = constantSizeProperyAccess(_.isDefined)
    def isEmpty: RCosted[Boolean] = constantSizeProperyAccess(_.isEmpty)
  }

  object OptionCoster extends CostingHandler[WOption[Any]]((obj, m, args) => new OptionCoster[Any](obj, m, args))


  class CollCoster[T](obj: RCosted[Coll[T]], method: SMethod, args: Seq[RCosted[_]]) extends Coster[Coll[T]](obj, method, args) {
    import Coll._
    implicit val eT = obj.elem.eVal.eItem

    def indices(): RCostedColl[Int] =
      knownLengthCollProperyAccess(_.indices, asSizeColl(obj.size).sizes.length)

    def getSizePropertyMethod[B](mc: MethodCall): RSize[T] => RColl[Size[B]] = {
      ???
    }

    def flatMap[B](fC: RCosted[T => Coll[B]]): RCostedColl[B] = {
      val f = fC.value
      f match {
        // Pattern: xs.flatMap(x => x.property)
        case Def(Lambda(l,_,_,Def(mc @ MethodCall(x, m, Nil, _)))) if x == l.x =>
          val sObj = asSizeColl(obj.size)
          val sizes: RColl[Size[B]] = sObj.sizes.flatMap(fun { s: RSize[T] =>
            val sizeProp = getSizePropertyMethod[B](mc)
            sizeProp(s)
          })
          val values = obj.value.flatMap(f)
          val costs = colBuilder.replicate(sizes.length, 0)
          RCCostedColl(values, costs, sizes, opCost(costOfArgs, costOf(method)))
        case _ =>
          !!!(s"Unsupported lambda in flatMap: allowed usage `xs.flatMap(x => x.property)`")
      }
    }

    def indexOf(elem: RCosted[T], from: RCosted[Int]): RCosted[Int] = {
      val c = opCost(costOfArgs, perKbCostOf(method, obj.size.dataSize))
      RCCostedPrim(obj.value.indexOf(elem.value, from.value), c, SizeInt)
    }

    def segmentLength(p: RCosted[T => Boolean], from: RCosted[Int]): RCosted[Int] = {
//      val pCost: Rep[((Int, Size[A])) => Int] = asCostedFunc(p).func.sliceCost
      // TODO costing rule should be more accurate
      val c = opCost(costOfArgs, costOf(method))
      RCCostedPrim(obj.value.segmentLength(p.value, from.value), c, SizeInt)
    }

    def indexWhere(p: RCosted[T => Boolean], from: RCosted[Int]): RCosted[Int] = {
      // TODO costing rule should be more accurate
      val c = opCost(costOfArgs, costOf(method))
      RCCostedPrim(obj.value.indexWhere(p.value, from.value), c, SizeInt)
    }

    def lastIndexWhere(p: RCosted[T => Boolean], end: RCosted[Int]): RCosted[Int] = {
      // TODO costing rule should be more accurate
      val c = opCost(costOfArgs, costOf(method))
      RCCostedPrim(obj.value.lastIndexWhere(p.value, end.value), c, SizeInt)
    }

    def zip[B](ys: RCosted[Coll[B]]): RCosted[Coll[(T, B)]] = {
      implicit val eB = ys.elem.eVal.eItem
      val values = obj.value.zip(ys.value)
      val xsC = asCostedColl(obj)
      val ysC = asCostedColl(ys)
      // TODO optimize: it make sence to add more high level operations to avoid building large graphs
      val costs = xsC.costs.zip(ysC.costs).map(fun { in: Rep[(Int,Int)] => in._1 + in._2 })
      val sizes = xsC.sizes.zip(ysC.sizes).map(fun { in: Rep[(Size[T],Size[B])] => RCSizePair(in._1, in._2): RSize[(T,B)] })
      val c = opCost(costOfArgs, costOf(method))
      RCCostedColl(values, costs, sizes, c)
    }

    def partition(pred: RCosted[T => Boolean]): RCosted[(Coll[T], Coll[T])] = {
      // TODO costing rule should be more accurate
      val xsC = asCostedColl(obj)
      val Pair(lvalues, rvalues) = xsC.value.partition(pred.value)
      val costs = xsC.costs
      val sizes = xsC.sizes
      val c = opCost(costOfArgs, costOf(method))
      RCCostedPair(
        RCCostedColl(lvalues, costs, sizes, CostTable.newCollValueCost),
        RCCostedColl(rvalues, costs, sizes, CostTable.newCollValueCost), c)
    }

    def patch(from: RCosted[Int], patch: RCosted[Coll[T]], replaced: RCosted[Int]): RCosted[Coll[T]] = {
      val xsC = asCostedColl(obj)
      val patchC = asCostedColl(patch)
      val values = xsC.value.patch(from.value, patch.value, replaced.value)
      val sizes = xsC.sizes.append(patchC.sizes)
      val costs = xsC.costs.append(patchC.costs)
      val c = opCost(costOfArgs, costOf(method)) // TODO costing rule should be more accurate
      RCCostedColl(values, costs, sizes, c)
    }

    def updated(index: RCosted[Int], elem: RCosted[T]): RCosted[Coll[T]] = {
      val xsC = asCostedColl(obj)
      val c = opCost(costOfArgs, costOf(method))
      RCCostedColl(xsC.value.updated(index.value, elem.value), xsC.costs, xsC.sizes, c)
    }

    def updateMany(indexes: RCosted[Coll[Int]], values: RCosted[Coll[T]]): RCosted[Coll[T]] = {
      val xsC = asCostedColl(obj)
      val c = opCost(costOfArgs, perKbCostOf(method, values.size.dataSize))  // TODO costing rule should be more accurate with sizes
      RCCostedColl(xsC.value.updateMany(indexes.value, values.value), xsC.costs, xsC.sizes, c)
    }
  }

  object CollCoster extends CostingHandler[Coll[Any]]((obj, m, args) => new CollCoster[Any](obj, m, args))

  class SigmaDslBuilderCoster(obj: RCosted[SigmaDslBuilder], method: SMethod, args: Seq[RCosted[_]]) extends Coster[SigmaDslBuilder](obj, method, args){
    import PreHeader._

    def groupGenerator() = groupElementProperyAccess(_.groupGenerator)
  }

  object SigmaDslBuilderCoster extends CostingHandler[SigmaDslBuilder]((obj, m, args) => new SigmaDslBuilderCoster(obj, m, args))

}
