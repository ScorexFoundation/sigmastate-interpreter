package sigmastate.eval

import org.ergoplatform.SigmaConstants.{MaxBoxSize, MaxBoxSizeWithoutRefs, MaxPropositionBytes}
import org.ergoplatform.{ErgoLikeContext, SigmaConstants}
import scalan.{SigmaLibrary, MutableLazy}
import sigmastate._
import sigmastate.interpreter.CryptoConstants
import sigmastate.utxo.CostTable
import spire.syntax.all.cfor

trait CostingRules extends SigmaLibrary { IR: IRContext =>
  import Coll._
  import BigInt._
  import SigmaProp._
  import AvlTree._
  import GroupElement._
  import CollBuilder._
  import CostedBuilder._
  import Costed._
  import Size._
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
  import Header._

  /** Implements basic costing rule invocation mechanism.
    * Each MethodCall node of ErgoTree is costed using the same mechanism.
    * When MethodCall is matched during traverse of ErgoTree in `RuntimeCosting.evalNode`:
    * 1) the type of the receiver object is used to lookup the corresponding CostingHandler
    * 2) The apply method of CostingHandler is called to create the Coster
    * 3) When Coster is created, the costing-rule-method is looked up using reflection and then invoked.
    * 4) The result of costing-rule-method is returned as the result of MethodCall node costing.
    *
    * Instances of this class are typically singleton objects (see below).
    * @param createCoster  constructor of Coster for given parameters of MethodCall: obj, method, costedArgs, args.
    * @see Coster
    */
  abstract class CostingHandler[T](createCoster: (RCosted[T], SMethod, Seq[RCosted[_]], Seq[Sym]) => Coster[T]) {
    def apply(obj: RCosted[_], method: SMethod, costedArgs: Seq[RCosted[_]], args: Seq[Sym] = Nil): RCosted[_] = {
      val coster = createCoster(asCosted[T](obj), method, costedArgs, args)
      val costerClass = coster.getClass
      val parameterTypes = Array.fill(costedArgs.length + args.length)(classOf[Sym])
      val costerMethod = costerClass.getMethod(method.name, parameterTypes:_*)
      val res = costerMethod.invoke(coster, costedArgs ++ args:_*)
      res.asInstanceOf[RCosted[_]]
    }
  }

  /** Lazy values, which are immutable, but can be reset, so that the next time they are accessed
    * the expression is re-evaluated. Each value should be reset in onReset() method.
    * Accessing this lazy value is an order of magnitude faster than computing it from scratch. */
  private val _someIntZero = MutableLazy(SOME(IntZero))
  @inline def SomeIntZero = _someIntZero.value

  private val _selectFieldCost = MutableLazy(sigmaDslBuilder.CostModel.SelectField)
  @inline def selectFieldCost = _selectFieldCost.value

  private val _getRegisterCost = MutableLazy(sigmaDslBuilder.CostModel.GetRegister)
  @inline def getRegisterCost = _getRegisterCost.value

  private val _sizeUnit: LazyRep[Size[Unit]] = MutableLazy(costedBuilder.mkSizePrim(0L, UnitElement))
  @inline def SizeUnit: RSize[Unit] = _sizeUnit.value

  private val _sizeBoolean: LazyRep[Size[Boolean]] = MutableLazy(costedBuilder.mkSizePrim(1L, BooleanElement))
  @inline def SizeBoolean: RSize[Boolean] = _sizeBoolean.value

  private val _sizeByte: LazyRep[Size[Byte]] = MutableLazy(costedBuilder.mkSizePrim(1L, ByteElement))
  @inline def SizeByte: RSize[Byte] = _sizeByte.value

  private val _sizeShort: LazyRep[Size[Short]] = MutableLazy(costedBuilder.mkSizePrim(2L, ShortElement))
  @inline def SizeShort: RSize[Short] = _sizeShort.value

  private val _sizeInt: LazyRep[Size[Int]] = MutableLazy(costedBuilder.mkSizePrim(4L, IntElement))
  @inline def SizeInt: RSize[Int] = _sizeInt.value

  private val _sizeLong: LazyRep[Size[Long]] = MutableLazy(costedBuilder.mkSizePrim(8L, LongElement))
  @inline def SizeLong: RSize[Long] = _sizeLong.value

  private val _sizeBigInt: LazyRep[Size[BigInt]] = MutableLazy(costedBuilder.mkSizePrim(SBigInt.MaxSizeInBytes, element[BigInt]))
  @inline def SizeBigInt: RSize[BigInt] = _sizeBigInt.value

  private val _sizeSigmaProp: LazyRep[Size[SigmaProp]] = MutableLazy(costedBuilder.mkSizePrim(SSigmaProp.MaxSizeInBytes, element[SigmaProp]))
  @inline def SizeSigmaProposition: RSize[SigmaProp] = _sizeSigmaProp.value

  private val _sizeString: LazyRep[Size[String]] = MutableLazy(costedBuilder.mkSizePrim(256L, StringElement))
  @inline def SizeString: RSize[String] = _sizeString.value

  private val _sizeAvlTree: LazyRep[Size[AvlTree]] = MutableLazy(costedBuilder.mkSizePrim(AvlTreeData.TreeDataSize.toLong, element[AvlTree]))
  @inline def SizeAvlTree: RSize[AvlTree] = _sizeAvlTree.value

  private val _sizeGroupElement: LazyRep[Size[GroupElement]] = MutableLazy(costedBuilder.mkSizePrim(CryptoConstants.EncodedGroupElementLength.toLong, element[GroupElement]))
  @inline def SizeGroupElement: RSize[GroupElement] = _sizeGroupElement.value

  private val _wRTypeSigmaProp: LazyRep[WRType[SigmaProp]] = MutableLazy(liftElem(element[SigmaProp]))
  @inline def WRTypeSigmaProp: Ref[WRType[SigmaProp]] = _wRTypeSigmaProp.value

  class KnownCollInfo[T](len: Int, itemSizeCreator: => RSize[T]) {

    private val _length: LazyRep[Int] = MutableLazy { len: Ref[Int] }
    def length: Ref[Int] = _length.value

    private val _itemSize: LazyRep[Size[T]] = MutableLazy(itemSizeCreator)
    def itemSize: RSize[T] = _itemSize.value

    private val _sizesColl: LazyRep[Coll[Size[T]]] = MutableLazy {
      colBuilder.replicate(length, itemSize)
    }
    @inline def sizesColl: RColl[Size[T]] = _sizesColl.value

    private val _costZeros: LazyRep[Coll[Int]] = MutableLazy {
      colBuilder.replicate(length, IntZero)
    }
    @inline def costZeros: RColl[Int] = _costZeros.value

    private val _size: LazyRep[Size[Coll[T]]] = MutableLazy {
      costedBuilder.mkSizeColl(sizesColl)
    }
    @inline def size: RSize[Coll[T]] = _size.value

    def reset() = {
      _length.reset()
      _itemSize.reset()
      _sizesColl.reset()
      _costZeros.reset()
      _size.reset()
    }

    /** Helper to wrap the given collection into costed collection node with given accumulated cost.
      * All components are taken from this info object.
      * @param   coll         collection to be wrapped (aka costed)
      * @param   valuesCost   accumulated cost to be added to resulting node
      * @return               Costed node representing cost information of the input collection `coll`.
      */
    def mkCostedColl(coll: Ref[Coll[T]], valuesCost: Ref[Int]): Ref[CostedColl[T]] = {
      RCCostedColl(coll, costZeros, sizesColl, valuesCost)
    }
  }

  val HashInfo = new KnownCollInfo(CryptoConstants.hashLength, SizeByte)
  val BoxBytesInfo = new KnownCollInfo(MaxBoxSize.value, SizeByte)
  val BoxBytesWithoutRefsInfo = new KnownCollInfo(MaxBoxSizeWithoutRefs.value, SizeByte)
  val BoxPropositionBytesInfo = new KnownCollInfo(MaxPropositionBytes.value, SizeByte)
  val LongBytesInfo = new KnownCollInfo(8, SizeByte)
  val NonceBytesInfo = new KnownCollInfo(8, SizeByte)
  val VotesInfo = new KnownCollInfo(3, SizeByte)
  val SigmaPropBytesInfo = new KnownCollInfo(SSigmaProp.MaxSizeInBytes.toInt, SizeByte)
  val EncodedGroupElementInfo = new KnownCollInfo(CryptoConstants.EncodedGroupElementLength.toInt, SizeByte)
  val AvlTreeDigestInfo = new KnownCollInfo(AvlTreeData.DigestSize, SizeByte)

  val HeadersInfo = new KnownCollInfo(ErgoLikeContext.MaxHeaders,
      costedBuilder.mkSizePrim(Sized.SizeHeader.dataSize, element[Header]))

  val TokensInfo = new KnownCollInfo(SigmaConstants.MaxTokens.value,
      mkSizePair(HashInfo.size, SizeLong))



  protected override def onReset(): Unit = {
    super.onReset()
    // WARNING: every lazy value should be listed here, otherwise behavior after resetContext is undefined
    // and may lead to subtle bugs.
    Array(_someIntZero,
      _selectFieldCost, _getRegisterCost, _sizeUnit, _sizeBoolean, _sizeByte, _sizeShort,
      _sizeInt, _sizeLong, _sizeBigInt, _sizeSigmaProp, _sizeString,
      _sizeAvlTree, _sizeGroupElement, _wRTypeSigmaProp)
        .foreach(_.reset())
    Array(HashInfo, BoxBytesInfo, BoxBytesWithoutRefsInfo, BoxPropositionBytesInfo,
      LongBytesInfo, NonceBytesInfo, VotesInfo, SigmaPropBytesInfo, EncodedGroupElementInfo,
      AvlTreeDigestInfo, HeadersInfo, TokensInfo)
        .foreach(_.reset())
  }

  case class Cast[To](eTo: Elem[To], x: Ref[Def[_]]) extends BaseDef[To]()(eTo) {
    override def transform(t: Transformer) = Cast(eTo, t(x))
  }

  def tryCast[To](x: Ref[Def[_]])(implicit eTo: Elem[To]): Ref[To] = {
    if (eTo.getClass.isAssignableFrom(x.elem.getClass))
      asRep[To](x)
    else
      Cast(eTo, x)
  }

  def asCostedColl[T](collC: RCosted[Coll[T]]): Ref[CostedColl[T]] = {
    implicit val eT = collC.elem.eVal.eItem
    tryCast[CostedColl[T]](collC)
  }
  def asCostedPair[A,B](pC: RCosted[(A,B)]): Ref[CostedPair[A,B]] = {
    implicit val eA = pC.elem.eVal.eFst
    implicit val eB = pC.elem.eVal.eSnd
    tryCast[CostedPair[A,B]](pC)
  }
  def asCostedFunc[A,B](fC: RCosted[A => B]): Ref[CostedFunc[Unit,A,B]] = {
    implicit val eA = fC.elem.eVal.eDom
    implicit val eB = fC.elem.eVal.eRange
    tryCast[CostedFunc[Unit, A, B]](fC)(costedFuncElement(UnitElement, eA, eB))
  }
  def asSizeColl[T](collS: RSize[Coll[T]]): Ref[SizeColl[T]] = {
    implicit val eT = collS.elem.eVal.eItem
    tryCast[SizeColl[T]](collS)
  }
  def asSizePair[A, B](s: RSize[(A,B)]): Ref[SizePair[A,B]] = {
    implicit val eA = s.elem.eVal.eFst
    implicit val eB = s.elem.eVal.eSnd
    tryCast[SizePair[A,B]](s)
  }
  def asSizeOption[T](optS: RSize[WOption[T]]): Ref[SizeOption[T]] = {
    implicit val eA = optS.elem.eVal.eItem
    tryCast[SizeOption[T]](optS)
  }
  def asSizeBox(ctx: RSize[Box]): Ref[SizeBox] = tryCast[SizeBox](ctx)
  def asSizeContext(ctx: RSize[Context]): Ref[SizeContext] = tryCast[SizeContext](ctx)

  def SOME[A](x: Ref[A]): Ref[WOption[A]] = specialPredef.some(x)

  def mkSizeColl[T:Elem](len: Ref[Int]): Ref[Size[Coll[T]]] = {
    val sizes = colBuilder.replicate(len, costedBuilder.mkSizePrim(typeSize[T], element[T]): RSize[T])
    costedBuilder.mkSizeColl(sizes)
  }

  def mkSizeColl[T](len: Ref[Int], sItem: RSize[T]): Ref[Size[Coll[T]]] = {
    val sizes = colBuilder.replicate(len, sItem)
    costedBuilder.mkSizeColl(sizes)
  }

  def mkSizeOption[T](size: RSize[T]): Ref[Size[WOption[T]]] = costedBuilder.mkSizeOption(SOME(size))
  def mkSizePair[A, B](l: RSize[A], r: RSize[B]): Ref[Size[(A,B)]] = costedBuilder.mkSizePair(l, r)

  def mkCostedColl[T](values: RColl[T], costs: RColl[Int], sizes: RColl[Size[T]], valuesCost: Ref[Int]): RCostedColl[T] =
    costedBuilder.mkCostedColl(values, costs, sizes, valuesCost)

  def mkCostedOption[T](value: ROption[T], costOpt: ROption[Int], sizeOpt: ROption[Size[T]], accCost: Ref[Int]): RCostedOption[T] =
    costedBuilder.mkCostedOption(value, costOpt, sizeOpt, accCost)

  def mkCostedFunc[A,R](f: RFuncCosted[A,R], cost: Ref[Int], codeSize: Ref[Long], eArg: Elem[A], eRes: Elem[R]): Ref[CostedFunc[Unit, A, R]] = {
    val envC = RCCostedPrim((), IntZero, SizeUnit)
    val sFunc = costedBuilder.mkSizeFunc(SizeUnit, codeSize, eArg, eRes)
    RCCostedFunc(envC, f, cost, sFunc)
  }

  /** For each Sigma type there should be one Coster class (derived from this).
    * Each coster object implements a set of costing rules, one rule for each method of the corresponding Sigma type.
    * For example, BoxCoster is coster for Box type, it contains rules for all methods registered in SBox type descriptor.
    * This class defines generic costing helpers, to unify and simplify costing rules of individual methods.
    */
  abstract class Coster[T](obj: RCosted[T], method: SMethod, costedArgs: Seq[RCosted[_]], args: Seq[Sym]) {
    def costOfArgs: Seq[Ref[Int]] = {
      val len = costedArgs.length
      val res = new Array[Ref[Int]](1 + len)
      res(0) = obj.cost
      cfor(0)(_ < len, _ + 1) { i =>
        res(i + 1) = costedArgs(i).asInstanceOf[Ref[Costed[Any]]].cost
      }
      res
    }
    def sizeOfArgs: Ref[Long] = costedArgs.foldLeft(obj.size.dataSize)((s, e) => s + e.size.dataSize)

    def constantSizePropertyAccess[R](prop: Ref[T] => Ref[R]): RCosted[R] = {
      val value = prop(obj.value)
      withConstantSize(value, opCost(value, costOfArgs, selectFieldCost))
    }

    def knownSizePropertyAccess[R](prop: Ref[T] => Ref[R], size: RSize[R]): RCosted[R] = {
      val value = prop(obj.value)
      RCCostedPrim(value, opCost(value, costOfArgs, selectFieldCost), size)
    }

    def knownLengthCollPropertyAccess[R](prop: Ref[T] => Ref[Coll[R]], info: KnownCollInfo[R]): Ref[CostedColl[R]] = {
      val value = prop(obj.value)
      info.mkCostedColl(value, opCost(value, costOfArgs, selectFieldCost))
    }

    def digest32PropertyAccess(prop: Ref[T] => Ref[Coll[Byte]]): Ref[CostedColl[Byte]] =
      knownLengthCollPropertyAccess(prop, HashInfo)

    def groupElementPropertyAccess(prop: Ref[T] => Ref[GroupElement]): RCosted[GroupElement] =
      knownSizePropertyAccess(prop, SizeGroupElement)

    def bigIntPropertyAccess(prop: Ref[T] => Ref[BigInt]): RCosted[BigInt] =
      knownSizePropertyAccess(prop, SizeBigInt)

    def defaultPropertyAccess[R](prop: Ref[T] => Ref[R], propSize: RSize[T] => RSize[R]): RCosted[R] = {
      val value = prop(obj.value)
      RCCostedPrim(value, opCost(value, costOfArgs, selectFieldCost), propSize(obj.size))
    }

    def defaultOptionPropertyAccess[R: Elem](prop: Ref[T] => ROption[R], propSize: RSize[T] => RSize[WOption[R]], itemCost: Ref[Int]): RCostedOption[R] = {
      val v = prop(obj.value)
      val s = propSize(obj.size)
      RCCostedOption(v, SOME(itemCost), asSizeOption(s).sizeOpt, opCost(v, costOfArgs, selectFieldCost))
    }

    def defaultCollPropertyAccess[R: Elem](prop: Ref[T] => RColl[R], propSize: RSize[T] => RSize[Coll[R]], itemCost: Ref[Int]): RCostedColl[R] = {
      val v = prop(obj.value)
      val s = propSize(obj.size)
      val sizes = asSizeColl(s).sizes
      val costs = colBuilder.replicate(sizes.length, itemCost)
      RCCostedColl(v, costs, sizes, opCost(v, costOfArgs, selectFieldCost))
    }

    def boxPropertyAccess(prop: Ref[T] => Ref[Box], propSize: RSize[T] => RSize[Box]): RCosted[Box] = {
      val v = prop(obj.value)
      val c = opCost(v, costOfArgs, sigmaDslBuilder.CostModel.AccessBox)
      val s = propSize(obj.size)
      RCCostedPrim(v, c, s)
    }

  }

  /** Costing rules for SGroupElement methods */
  class GroupElementCoster(obj: RCosted[GroupElement], method: SMethod, costedArgs: Seq[RCosted[_]], args: Seq[Sym]) extends Coster[GroupElement](obj, method, costedArgs, args){
    import GroupElement._
    def getEncoded: RCosted[Coll[Byte]] =
      knownLengthCollPropertyAccess(_.getEncoded, EncodedGroupElementInfo)

    def negate: RCosted[GroupElement] = {
      val value = obj.value.negate
      RCCostedPrim(value, opCost(value, costOfArgs, costOf(method)), SizeGroupElement)
    }
  }

  /** CostingHandler for SGroupElement, see SGroupElement.coster */
  object GroupElementCoster extends CostingHandler[GroupElement]((obj, m, costedArgs, args) => new GroupElementCoster(obj, m, costedArgs, args))

  /** Costing rules for SAvlTree methods */
  class AvlTreeCoster(obj: RCosted[AvlTree], method: SMethod, costedArgs: Seq[RCosted[_]], args: Seq[Sym]) extends Coster[AvlTree](obj, method, costedArgs, args){
    import AvlTree._
    def digest() = knownLengthCollPropertyAccess(_.digest, AvlTreeDigestInfo)
    def enabledOperations() = constantSizePropertyAccess(_.enabledOperations)
    def keyLength() = constantSizePropertyAccess(_.keyLength)
    def valueLengthOpt() =
        defaultOptionPropertyAccess(_.valueLengthOpt, _ => mkSizeOption(SizeInt), IntZero)
    def isInsertAllowed() = constantSizePropertyAccess(_.isInsertAllowed)
    def isUpdateAllowed() = constantSizePropertyAccess(_.isUpdateAllowed)
    def isRemoveAllowed() = constantSizePropertyAccess(_.isRemoveAllowed)

    def updateDigest(newDigest: RCosted[Coll[Byte]]) = {
      val value = obj.value.updateDigest(newDigest.value)
      RCCostedPrim(value, opCost(value, costOfArgs, costOf(method)), obj.size)
    }
    def updateOperations(flags: RCosted[Byte]) = {
      val value = obj.value.updateOperations(flags.value)
      RCCostedPrim(value, opCost(value, costOfArgs, costOf(method)), obj.size)
    }
    def contains(key: RCosted[Coll[Byte]], proof: RCosted[Coll[Byte]]): RCosted[Boolean] = {
      val value = obj.value.contains(key.value, proof.value)
      withConstantSize(value, opCost(value, costOfArgs, perKbCostOf(method, sizeOfArgs)))
    }
    def get(key: RCosted[Coll[Byte]], proof: RCosted[Coll[Byte]]): RCosted[WOption[Coll[Byte]]] = {
      val value = obj.value.get(key.value, proof.value)
      val size = sizeOfArgs
      val c = opCost(value, costOfArgs, perKbCostOf(method, size))
      val res = RCCostedOption(value,
        SomeIntZero,
        specialPredef.some(proof.size),
        c)
      res
    }
    def getMany(_keys: RCosted[Coll[Coll[Byte]]], _proof: RCosted[Coll[Byte]]): RCosted[Coll[WOption[Coll[Byte]]]] = {
      val keysC = asCostedColl[Coll[Byte]](_keys)
      val proofC = asCostedColl[Byte](_proof)
      val nKeys = keysC.sizes.length

      val value = obj.value.getMany(keysC.value, proofC.value)
      val costs = colBuilder.replicate(nKeys, IntZero)
      val valuesCost = opCost(value, costOfArgs, perKbCostOf(method, sizeOfArgs))

      val treeValueLengthPerKey = proofC.sizes.length div nKeys
      val treeValueS = mkSizeColl[Byte](treeValueLengthPerKey)
      val sizes = colBuilder.replicate(nKeys, mkSizeOption(treeValueS))

      val res = RCCostedColl(value, costs, sizes, valuesCost)
      res
    }

    private def treeModifierMethod(meth: Ref[AvlTree] => Ref[WOption[AvlTree]]): RCosted[WOption[AvlTree]] = {
      val value = meth(obj.value)
      val size = sizeOfArgs
      RCCostedOption(value,
        specialPredef.some(IntZero),
        SOME(obj.size), opCost(value, costOfArgs, perKbCostOf(method, size)))
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

  object AvlTreeCoster extends CostingHandler[AvlTree]((obj, m, costedArgs, args) => new AvlTreeCoster(obj, m, costedArgs, args))

  /** Costing rules for SContext methods */
  class ContextCoster(obj: RCosted[Context], method: SMethod, costedArgs: Seq[RCosted[_]], args: Seq[Sym]) extends Coster[Context](obj, method, costedArgs, args){
    import Context._
    def boxCollProperty(prop: Ref[Context] => Ref[Coll[Box]], propSize: Ref[SizeContext] => RSize[Coll[Box]]) = {
      defaultCollPropertyAccess(prop, ctxS => propSize(asSizeContext(ctxS)), sigmaDslBuilder.CostModel.AccessBox)
    }
    def headers() = {
      knownLengthCollPropertyAccess(_.headers, HeadersInfo)
    }
    def preHeader() = constantSizePropertyAccess(_.preHeader)

    def dataInputs(): RCostedColl[Box] = {
      boxCollProperty(_.dataInputs, _.dataInputs)
    }

    def OUTPUTS: RCostedColl[Box] = {
      boxCollProperty(_.OUTPUTS, _.outputs)
    }

    def INPUTS: RCostedColl[Box] = {
      boxCollProperty(_.INPUTS, _.inputs)
    }

    def HEIGHT: RCosted[Int] = constantSizePropertyAccess(_.HEIGHT)

    def SELF: RCosted[Box] = boxPropertyAccess(_.SELF, asSizeContext(_).selfBox)

    def LastBlockUtxoRootHash: RCosted[AvlTree] =
      knownSizePropertyAccess(_.LastBlockUtxoRootHash, SizeAvlTree)

    def minerPubKey: RCostedColl[Byte] =
      knownLengthCollPropertyAccess(_.minerPubKey, EncodedGroupElementInfo)

    def getVar[T](id: RCosted[Byte])(implicit tT: Ref[WRType[T]]): RCostedOption[T] = { ???
//      defaultOptionPropertyAccess(_.getVar(id.value)(tT.eA), asSizeContext(_).reg)
//      val opt = ctx.getVar(id)(cT)
//      dsl.costOption(opt, dsl.CostModel.GetVar)
    }

    def selfBoxIndex: RCosted[Int] = constantSizePropertyAccess(_.selfBoxIndex)

  }

  object ContextCoster extends CostingHandler[Context]((obj, m, costedArgs, args) => new ContextCoster(obj, m, costedArgs, args))

  /** Costing rules for SBox methods */
  class BoxCoster(obj: RCosted[Box], method: SMethod, costedArgs: Seq[RCosted[_]], args: Seq[Sym]) extends Coster[Box](obj, method, costedArgs, args){
    import Box._

    def creationInfo: RCosted[(Int, Coll[Byte])] = {
      val info = obj.value.creationInfo
      val l = RCCostedPrim(info._1, IntZero, SizeInt)
      val r = RCCostedColl(info._2, HashInfo.costZeros, HashInfo.sizesColl, IntZero)
      val cost = opCost(Pair(l, r), Array(obj.cost), getRegisterCost)
      RCCostedPair(l, r, cost)
    }

    def tokens() = {
      val value = obj.value.tokens
      TokensInfo.mkCostedColl(value, opCost(value, costOfArgs, costOf(method)))
    }

    def getReg[T](i: RCosted[Int])(implicit tT: Ref[WRType[T]]): RCosted[WOption[T]] = {
      val sBox = asSizeBox(obj.size)
      implicit val elem = tT.eA
      val valueOpt = obj.value.getReg(i.value)(elem)
      val sReg = asSizeOption(sBox.getReg(downcast[Byte](i.value))(elem))
      RCCostedOption(valueOpt, SomeIntZero, sReg.sizeOpt, opCost(valueOpt, Array(obj.cost), getRegisterCost))
    }
  }

  object BoxCoster extends CostingHandler[Box]((obj, m, costedArgs, args) => new BoxCoster(obj, m, costedArgs, args))

  /** Costing rules for SHeader methods */
  class HeaderCoster(obj: RCosted[Header], method: SMethod, costedArgs: Seq[RCosted[_]], args: Seq[Sym]) extends Coster[Header](obj, method, costedArgs, args){
    import Header._

    def id() = digest32PropertyAccess(_.id)

    def version() = constantSizePropertyAccess(_.version)

    def parentId() = digest32PropertyAccess(_.parentId)

    def ADProofsRoot() = digest32PropertyAccess(_.ADProofsRoot)

    def stateRoot() = knownSizePropertyAccess(_.stateRoot, SizeAvlTree)

    def transactionsRoot() = digest32PropertyAccess(_.transactionsRoot)

    def timestamp() = constantSizePropertyAccess(_.timestamp)

    def nBits() = constantSizePropertyAccess(_.nBits)

    def height() = constantSizePropertyAccess(_.height)

    def extensionRoot() = digest32PropertyAccess(_.extensionRoot)

    def minerPk() = groupElementPropertyAccess(_.minerPk)

    def powOnetimePk() = groupElementPropertyAccess(_.powOnetimePk)

    def powNonce() = knownLengthCollPropertyAccess(_.powNonce, NonceBytesInfo)

    def powDistance() = bigIntPropertyAccess(_.powDistance)

    def votes() = knownLengthCollPropertyAccess(_.votes, VotesInfo)
  }

  object HeaderCoster extends CostingHandler[Header]((obj, m, costedArgs, args) => new HeaderCoster(obj, m, costedArgs, args))

  /** Costing rules for SPreHeader methods */
  class PreHeaderCoster(obj: RCosted[PreHeader], method: SMethod, costedArgs: Seq[RCosted[_]], args: Seq[Sym]) extends Coster[PreHeader](obj, method, costedArgs, args){
    import PreHeader._

    def version() = constantSizePropertyAccess(_.version)

    def parentId() = digest32PropertyAccess(_.parentId)

    def timestamp() = constantSizePropertyAccess(_.timestamp)

    def nBits() = constantSizePropertyAccess(_.nBits)

    def height() = constantSizePropertyAccess(_.height)

    def minerPk() = groupElementPropertyAccess(_.minerPk)

    def votes() = knownLengthCollPropertyAccess(_.votes, VotesInfo)
  }

  object PreHeaderCoster extends CostingHandler[PreHeader]((obj, m, costedArgs, args) => new PreHeaderCoster(obj, m, costedArgs, args))

  /** Costing rules for SOption methods (see object SOption) */
  class OptionCoster[T](obj: RCosted[WOption[T]], method: SMethod, costedArgs: Seq[RCosted[_]], args: Seq[Sym]) extends Coster[WOption[T]](obj, method, costedArgs, args){
    import WOption._
    implicit val eT = obj.elem.eVal.eItem
    def get(): RCosted[T] = defaultPropertyAccess(_.get, asSizeOption(_).sizeOpt.get)

    def getOrElse(default: RCosted[T]): RCosted[T] = {
      val v = obj.value.getOrElse(default.value)
      val c = opCost(v, costOfArgs, selectFieldCost)
      val s = asSizeOption(obj.size).sizeOpt.getOrElse(default.size)
      RCCostedPrim(v, c, s)
    }

    def isDefined: RCosted[Boolean] = constantSizePropertyAccess(_.isDefined)
    def isEmpty: RCosted[Boolean] = constantSizePropertyAccess(_.isEmpty)

    def map[B](_f: RCosted[T => B]): RCosted[WOption[B]] = {
      val f = asCostedFunc[T,B](_f)
      val calcF = f.sliceCalc
      val costF = f.sliceCost
      val sizeF = f.sliceSize
      val v = obj.value.map(calcF)
      val sizeOpt = asSizeOption(obj.size).sizeOpt
      val c = costF(Pair(obj.cost,  sizeOpt.get))
      val s = sizeOpt.map(sizeF)
      RCCostedOption(v, SOME(c), s, opCost(v, costOfArgs, costOf(method)))
    }

    def filter(_f: RCosted[T => Boolean]): RCosted[WOption[T]] = {
      val f = asCostedFunc[T,Boolean](_f)
      val calcF = f.sliceCalc
      val costF = f.sliceCost
      val v = obj.value.filter(calcF)
      val sizeOpt = asSizeOption(obj.size).sizeOpt
      val c = costF(Pair(obj.cost,  sizeOpt.get))
      RCCostedOption(v, SOME(c), sizeOpt, opCost(v, costOfArgs, costOf(method)))
    }
  }

  object OptionCoster extends CostingHandler[WOption[Any]]((obj, m, costedArgs, args) => new OptionCoster[Any](obj, m, costedArgs, args))

  /** Costing rules for SCollection methods (see object SCollection) */
  class CollCoster[T](obj: RCosted[Coll[T]], method: SMethod, costedArgs: Seq[RCosted[_]], args: Seq[Sym]) extends Coster[Coll[T]](obj, method, costedArgs, args) {
    import Coll._
    implicit val eT = obj.elem.eVal.eItem

    def indices(): RCostedColl[Int] = {
      val value = obj.value.indices
      mkCostedColl(value, asSizeColl(obj.size).sizes.length, opCost(value, costOfArgs, selectFieldCost))
    }

    def map[B](_f: RCosted[T => B]): RCosted[Coll[B]] = {
      val xs = asCostedColl(obj)
      val f = asCostedFunc[T,B](_f)
      val calcF = f.sliceCalc
      val costF = f.sliceCost
      val sizeF = f.sliceSize
      val vals = xs.values.map(calcF)
      val sizes = xs.sizes
      val len = sizes.length
      val zeros = colBuilder.replicate(len, IntZero)

      val eB = f.elem.eVal.eRange
      val resSizes = if (eB.isConstantSize) {
        colBuilder.replicate(xs.sizes.length, constantTypeSize(eB): RSize[B])
      } else {
        xs.sizes.map(sizeF)
      }
      val isConstSize = eT.sourceType.isConstantSize
      val mapperCost = if (isConstSize) {
        val mcost: Ref[Int] = Apply(costF, Pair(IntZero, constantTypeSize(eT)), false)
        len * (mcost + CostTable.lambdaInvoke)
      } else {
        zeros.zip(sizes).map(costF).sum(intPlusMonoid) + len * CostTable.lambdaInvoke
      }
      RCCostedColl(vals, zeros, resSizes, opCost(vals, costOfArgs, costOf(method) + mapperCost))
    }

    def filter(_f: RCosted[T => Boolean]): RCosted[Coll[T]] = {
      val xs = asCostedColl(obj)
      val f = asCostedFunc[T,Boolean](_f)
      val calcF = f.sliceCalc
      val costF = f.sliceCost
      val vals = xs.values.filter(calcF)
      val sizes = xs.sizes
      val len = sizes.length
      val zeros = colBuilder.replicate(len, IntZero)
      val isConstSize = eT.sourceType.isConstantSize
      val predicateCost = if (isConstSize) {
        val predCost: Ref[Int] = Apply(costF, Pair(IntZero, constantTypeSize(eT)), false)
        len * (predCost + CostTable.lambdaInvoke)
      } else {
        zeros.zip(sizes).map(costF).sum(intPlusMonoid) + len * CostTable.lambdaInvoke
      }
      RCCostedColl(vals, zeros, sizes, opCost(vals, costOfArgs, costOf(method) + predicateCost))
    }

    def flatMap[B](fC: RCosted[T => Coll[B]]): RCostedColl[B] = {
      val fV = fC.value
      fV match {
        // Pattern: xs.flatMap(x => x.property)
        case Def(Lambda(l,_,_,Def(mc @ MethodCall(x, m, Nil, _)))) if x == l.x =>
          val cfC = asCostedFunc[T, Coll[B]](fC)
          val calcF = cfC.sliceCalc
          val sizeF = cfC.sliceSize
          val costF = cfC.sliceCost
          val xs = asCostedColl(obj)
          val vals = xs.values.flatMap(calcF)
          val sizes = xs.sizes
          val len = sizes.length
          val resSizes: RColl[Size[B]] = sizes.flatMap(fun { s: RSize[T] =>
            asSizeColl(sizeF(s)).sizes
          })

          val isConstSize = eT.sourceType.isConstantSize
          val mapperCost = if (isConstSize) {
            val mcost: Ref[Int] = Apply(costF, Pair(IntZero, constantTypeSize(eT)), false)
            len * (mcost + CostTable.lambdaInvoke)
          } else {
            colBuilder.replicate(len, IntZero)
              .zip(sizes)
              .map(costF)
              .sum(intPlusMonoid) + len * CostTable.lambdaInvoke
          }
          val zeros = colBuilder.replicate(resSizes.length, IntZero)
          RCCostedColl(vals, zeros, resSizes, opCost(vals, costOfArgs, costOf(method) + mapperCost))
        case _ =>
          !!!(s"Unsupported lambda in flatMap: allowed usage `xs.flatMap(x => x.property)`")
      }
    }

    def indexOf(elem: RCosted[T], from: RCosted[Int]): RCosted[Int] = {
      val value = obj.value.indexOf(elem.value, from.value)
      val c = opCost(value, costOfArgs, perKbCostOf(method, obj.size.dataSize))
      RCCostedPrim(value, c, SizeInt)
    }

    def zip[B](ys: RCosted[Coll[B]]): RCosted[Coll[(T, B)]] = {
      implicit val eB = ys.elem.eVal.eItem
      val values = obj.value.zip(ys.value)
      val xsC = asCostedColl(obj)
      val ysC = asCostedColl(ys)
      // TODO optimize: it make sence to add more high level operations to avoid building large graphs
      val costs = xsC.costs.zip(ysC.costs).map(fun { in: Ref[(Int,Int)] => in._1 + in._2 })
      val sizes = xsC.sizes.zip(ysC.sizes).map(fun { in: Ref[(Size[T],Size[B])] => RCSizePair(in._1, in._2): RSize[(T,B)] })
      val c = opCost(values, costOfArgs, costOf(method))
      RCCostedColl(values, costs, sizes, c)
    }

    def patch(from: RCosted[Int], patch: RCosted[Coll[T]], replaced: RCosted[Int]): RCosted[Coll[T]] = {
      val xsC = asCostedColl(obj)
      val patchC = asCostedColl(patch)
      val values = xsC.value.patch(from.value, patch.value, replaced.value)
      val sizes = xsC.sizes.append(patchC.sizes)
      val costs = xsC.costs.append(patchC.costs)
      val c = opCost(values, costOfArgs, costOf(method))
      RCCostedColl(values, costs, sizes, c)
    }

    def updated(index: RCosted[Int], elem: RCosted[T]): RCosted[Coll[T]] = {
      val xsC = asCostedColl(obj)
      val v = xsC.value.updated(index.value, elem.value)
      val c = opCost(v, costOfArgs, costOf(method))
      RCCostedColl(v, xsC.costs, xsC.sizes, c)
    }

    def updateMany(indexes: RCosted[Coll[Int]], values: RCosted[Coll[T]]): RCosted[Coll[T]] = {
      val xsC = asCostedColl(obj)
      val v = xsC.value.updateMany(indexes.value, values.value)
      val c = opCost(v, costOfArgs, perKbCostOf(method, values.size.dataSize))
      RCCostedColl(v, xsC.costs, xsC.sizes, c)
    }

  }

  object CollCoster extends CostingHandler[Coll[Any]]((obj, m, costedArgs, args) => new CollCoster[Any](obj, m, costedArgs, args))

  /** Costing rules for SGlobal methods */
  class SigmaDslBuilderCoster(obj: RCosted[SigmaDslBuilder], method: SMethod, costedArgs: Seq[RCosted[_]], args: Seq[Sym]) extends Coster[SigmaDslBuilder](obj, method, costedArgs, args){

    def groupGenerator() = groupElementPropertyAccess(_.groupGenerator)
  }

  object SigmaDslBuilderCoster extends CostingHandler[SigmaDslBuilder]((obj, m, costedArgs, args) => new SigmaDslBuilderCoster(obj, m, costedArgs, args))

}
