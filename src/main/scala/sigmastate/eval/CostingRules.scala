package sigmastate.eval

import org.ergoplatform.ErgoLikeContext
import scalan.{SigmaLibrary, MutableLazy}
import sigmastate._
import sigmastate.Values._
import sigmastate.SType.AnyOps
import sigmastate.interpreter.CryptoConstants
import sigmastate.utxo.CostTable

trait CostingRules extends SigmaLibrary { IR: RuntimeCosting =>
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
  abstract class CostingHandler[T](createCoster: (RCosted[T], SMethod, Seq[RCosted[_]]) => Coster[T]) {
    def apply(obj: RCosted[_], method: SMethod, costedArgs: Seq[RCosted[_]], args: Seq[Sym] = Nil): RCosted[_] = {
      val coster = createCoster(asCosted[T](obj), method, costedArgs)  // TODO use also args to create Coster
      val costerClass = coster.getClass
      val parameterTypes = Array.fill(costedArgs.length + args.length)(classOf[Sym])
      val costerMethod = costerClass.getMethod(method.name, parameterTypes:_*)
      val res = costerMethod.invoke(coster, costedArgs ++ args:_*)
      res.asInstanceOf[RCosted[_]]
    }
  }

  /** Lazy values, which are immutable, but can be reset, so that the next time they are accessed
    * the expression is re-evaluated. Each value should be reset in onReset() method. */
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

  private val _sizeString: LazyRep[Size[String]] = MutableLazy(costedBuilder.mkSizePrim(256L, StringElement))
  @inline def SizeString: RSize[String] = _sizeString.value

  private val _sizeAvlTree: LazyRep[Size[AvlTree]] = MutableLazy(costedBuilder.mkSizePrim(AvlTreeData.TreeDataSize.toLong, element[AvlTree]))
  @inline def SizeAvlTree: RSize[AvlTree] = _sizeAvlTree.value

  private val _sizeGroupElement: LazyRep[Size[GroupElement]] = MutableLazy(costedBuilder.mkSizePrim(CryptoConstants.EncodedGroupElementLength.toLong, element[GroupElement]))
  @inline def SizeGroupElement: RSize[GroupElement] = _sizeGroupElement.value

  private val _wRTypeSigmaProp: LazyRep[WRType[SigmaProp]] = MutableLazy(liftElem(element[SigmaProp]))
  @inline def WRTypeSigmaProp: Rep[WRType[SigmaProp]] = _wRTypeSigmaProp.value

  private val _sizeHashBytes: LazyRep[Size[Coll[Byte]]] = MutableLazy {
    val len: Rep[Int] = CryptoConstants.hashLength
    val sizes = colBuilder.replicate(len, SizeByte)
    costedBuilder.mkSizeColl(sizes)
  }
  @inline def SizeHashBytes: RSize[Coll[Byte]] = _sizeHashBytes.value

  /** */
  protected override def onReset(): Unit = {
    super.onReset()
    // WARNING: every lazy value should be listed here, otherwise bevavior after resetContext is undefined and may throw.
    Array(_selectFieldCost, _getRegisterCost, _sizeUnit, _sizeBoolean, _sizeByte, _sizeShort,
      _sizeInt, _sizeLong, _sizeBigInt, _sizeString, _sizeAvlTree, _sizeGroupElement, _wRTypeSigmaProp, _sizeHashBytes)
        .foreach(_.reset())
  }

  def mkSizeSigmaProp(size: Rep[Long]): RSize[SigmaProp] = costedBuilder.mkSizePrim(size, WRTypeSigmaProp)

  def SizeOfSigmaProp(p: SSigmaProp): RSize[SigmaProp] = mkSizeSigmaProp(SSigmaProp.dataSize(p.asWrappedType))

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

  def SOME[A](x: Rep[A]): Rep[WOption[A]] = specialPredef.some(x)

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

  /** For each Sigma type there should be one Coster class (derived from this).
    * Each coster object implements a set of costing rules, one rule for each method of the corresponding Sigma type.
    * For example, BoxCoster is coster for Box type, it contains rules for all methods registered in SBox type descriptor.
    * This class defines generic costing helpers, to unify and simplify costing rules of individual methods.
    */
  abstract class Coster[T](obj: RCosted[T], method: SMethod, args: Seq[RCosted[_]]) {
    def costOfArgs: Seq[Rep[Int]] = (obj +: args).map(_.cost)
    def sizeOfArgs: Rep[Long] = args.foldLeft(obj.size.dataSize)({ case (s, e) => s + e.size.dataSize })

    def constantSizePropertyAccess[R](prop: Rep[T] => Rep[R]): RCosted[R] = {
      val value = prop(obj.value)
      withConstantSize(value, opCost(value, costOfArgs, selectFieldCost))
    }

    def knownSizePropertyAccess[R](prop: Rep[T] => Rep[R], size: RSize[R]): RCosted[R] = {
      val value = prop(obj.value)
      RCCostedPrim(value, opCost(value, costOfArgs, selectFieldCost), size)
    }

    def knownLengthCollPropertyAccess[R](prop: Rep[T] => Rep[Coll[R]], len: Rep[Int]): Rep[CostedColl[R]] = {
      val value = prop(obj.value)
      mkCostedColl(value, len, opCost(value, costOfArgs, selectFieldCost))
    }

    def digest32PropertyAccess(prop: Rep[T] => Rep[Coll[Byte]]): Rep[CostedColl[Byte]] =
      knownLengthCollPropertyAccess(prop, CryptoConstants.hashLength)

    def groupElementPropertyAccess(prop: Rep[T] => Rep[GroupElement]): RCosted[GroupElement] =
      knownSizePropertyAccess(prop, SizeGroupElement)

    def bigIntPropertyAccess(prop: Rep[T] => Rep[BigInt]): RCosted[BigInt] =
      knownSizePropertyAccess(prop, SizeBigInt)

    def defaultPropertyAccess[R](prop: Rep[T] => Rep[R], propSize: RSize[T] => RSize[R]): RCosted[R] = {
      val value = prop(obj.value)
      RCCostedPrim(value, opCost(value, costOfArgs, selectFieldCost), propSize(obj.size))
    }

    def defaultOptionPropertyAccess[R: Elem](prop: Rep[T] => ROption[R], propSize: RSize[T] => RSize[WOption[R]], itemCost: Rep[Int]): RCostedOption[R] = {
      val v = prop(obj.value)
      val s = propSize(obj.size)
      RCCostedOption(v, SOME(itemCost), asSizeOption(s).sizeOpt, opCost(v, costOfArgs, selectFieldCost))
    }

    def defaultCollPropertyAccess[R: Elem](prop: Rep[T] => RColl[R], propSize: RSize[T] => RSize[Coll[R]], itemCost: Rep[Int]): RCostedColl[R] = {
      val v = prop(obj.value)
      val s = propSize(obj.size)
      val sizes = asSizeColl(s).sizes
      val costs = colBuilder.replicate(sizes.length, itemCost)
      RCCostedColl(v, costs, sizes, opCost(v, costOfArgs, selectFieldCost))
    }

    def boxPropertyAccess(prop: Rep[T] => Rep[Box], propSize: RSize[T] => RSize[Box]): RCosted[Box] = {
      val v = prop(obj.value)
      val c = opCost(v, costOfArgs, sigmaDslBuilder.CostModel.AccessBox)
      val s = propSize(obj.size)
      RCCostedPrim(v, c, s)
    }

  }

  /** Costing rules for SGroupElement methods */
  class GroupElementCoster(obj: RCosted[GroupElement], method: SMethod, args: Seq[RCosted[_]]) extends Coster[GroupElement](obj, method, args){
    import GroupElement._
    def getEncoded: RCosted[Coll[Byte]] =
      knownLengthCollPropertyAccess(_.getEncoded, CryptoConstants.EncodedGroupElementLength.toInt)

    def negate: RCosted[GroupElement] = {
      val value = obj.value.negate
      RCCostedPrim(value, opCost(value, costOfArgs, costOf(method)), SizeGroupElement)
    }
  }

  /** CostingHandler for SGroupElement, see SGroupElement.coster */
  object GroupElementCoster extends CostingHandler[GroupElement]((obj, m, args) => new GroupElementCoster(obj, m, args))

  /** Costing rules for SAvlTree methods */
  class AvlTreeCoster(obj: RCosted[AvlTree], method: SMethod, args: Seq[RCosted[_]]) extends Coster[AvlTree](obj, method, args){
    import AvlTree._
    def digest() = knownLengthCollPropertyAccess(_.digest, AvlTreeData.DigestSize)
    def enabledOperations() = constantSizePropertyAccess(_.enabledOperations)
    def keyLength() = constantSizePropertyAccess(_.keyLength)
    def valueLengthOpt() = defaultOptionPropertyAccess(_.valueLengthOpt, _ => mkSizeOption(SizeInt), 0)
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
        specialPredef.some(0),
        specialPredef.some(proof.size),
        c)
      res
    }
    def getMany(_keys: RCosted[Coll[Coll[Byte]]], _proof: RCosted[Coll[Byte]]): RCosted[Coll[WOption[Coll[Byte]]]] = {
      val keysC = asCostedColl[Coll[Byte]](_keys)
      val proofC = asCostedColl[Byte](_proof)
      val nKeys = keysC.sizes.length

      val value = obj.value.getMany(keysC.value, proofC.value)
      val costs = colBuilder.replicate(nKeys, 0)
      val valuesCost = opCost(value, costOfArgs, perKbCostOf(method, sizeOfArgs))

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
        specialPredef.some(0),
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

  object AvlTreeCoster extends CostingHandler[AvlTree]((obj, m, args) => new AvlTreeCoster(obj, m, args))

  /** Costing rules for SContext methods */
  class ContextCoster(obj: RCosted[Context], method: SMethod, args: Seq[RCosted[_]]) extends Coster[Context](obj, method, args){
    import Context._
    def boxCollProperty(prop: Rep[Context] => Rep[Coll[Box]], propSize: Rep[SizeContext] => RSize[Coll[Box]]) = {
      defaultCollPropertyAccess(prop, ctxS => propSize(asSizeContext(ctxS)), sigmaDslBuilder.CostModel.AccessBox)
    }
    def headers() = {
      knownLengthCollPropertyAccess(_.headers, ErgoLikeContext.MaxHeaders)
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
      knownLengthCollPropertyAccess(_.minerPubKey, CryptoConstants.EncodedGroupElementLength.toInt)

    def getVar[T](id: RCosted[Byte])(implicit tT: Rep[WRType[T]]): RCostedOption[T] = { ???
//      defaultOptionPropertyAccess(_.getVar(id.value)(tT.eA), asSizeContext(_).reg)
//      val opt = ctx.getVar(id)(cT)
//      dsl.costOption(opt, dsl.CostModel.GetVar)
    }

    def selfBoxIndex: RCosted[Int] = constantSizePropertyAccess(_.selfBoxIndex)

  }

  object ContextCoster extends CostingHandler[Context]((obj, m, args) => new ContextCoster(obj, m, args))

  /** Costing rules for SBox methods */
  class BoxCoster(obj: RCosted[Box], method: SMethod, args: Seq[RCosted[_]]) extends Coster[Box](obj, method, args){
    import Box._

    def creationInfo: RCosted[(Int, Coll[Byte])] = {
      val info = obj.value.creationInfo
      val l = RCCostedPrim(info._1, 0, SizeInt)
      val r = mkCostedColl(info._2, CryptoConstants.hashLength, 0)
      val cost = opCost(Pair(l, r), Seq(obj.cost), getRegisterCost)
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
      RCCostedColl(tokens, costs, sizes, opCost(tokens, Seq(obj.cost), costOf(method)))
    }

    def getReg[T](i: RCosted[Int])(implicit tT: Rep[WRType[T]]): RCosted[WOption[T]] = {
      val sBox = asSizeBox(obj.size)
      implicit val elem = tT.eA
      val valueOpt = obj.value.getReg(i.value)(elem)
      val sReg = asSizeOption(sBox.getReg(downcast[Byte](i.value))(elem))
      RCCostedOption(valueOpt, SOME(0), sReg.sizeOpt, opCost(valueOpt, Seq(obj.cost), getRegisterCost))
    }
  }

  object BoxCoster extends CostingHandler[Box]((obj, m, args) => new BoxCoster(obj, m, args))

  /** Costing rules for SHeader methods */
  class HeaderCoster(obj: RCosted[Header], method: SMethod, args: Seq[RCosted[_]]) extends Coster[Header](obj, method, args){
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

    def powNonce() = knownLengthCollPropertyAccess(_.powNonce, 8)

    def powDistance() = bigIntPropertyAccess(_.powDistance)

    def votes() = knownLengthCollPropertyAccess(_.votes, 3)
  }

  object HeaderCoster extends CostingHandler[Header]((obj, m, args) => new HeaderCoster(obj, m, args))

  /** Costing rules for SPreHeader methods */
  class PreHeaderCoster(obj: RCosted[PreHeader], method: SMethod, args: Seq[RCosted[_]]) extends Coster[PreHeader](obj, method, args){
    import PreHeader._

    def version() = constantSizePropertyAccess(_.version)

    def parentId() = digest32PropertyAccess(_.parentId)

    def timestamp() = constantSizePropertyAccess(_.timestamp)

    def nBits() = constantSizePropertyAccess(_.nBits)

    def height() = constantSizePropertyAccess(_.height)

    def minerPk() = groupElementPropertyAccess(_.minerPk)

    def votes() = knownLengthCollPropertyAccess(_.votes, 3)
  }

  object PreHeaderCoster extends CostingHandler[PreHeader]((obj, m, args) => new PreHeaderCoster(obj, m, args))

  /** Costing rules for SOption methods (see object SOption) */
  class OptionCoster[T](obj: RCosted[WOption[T]], method: SMethod, args: Seq[RCosted[_]]) extends Coster[WOption[T]](obj, method, args){
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

  object OptionCoster extends CostingHandler[WOption[Any]]((obj, m, args) => new OptionCoster[Any](obj, m, args))

  /** Costing rules for SCollection methods (see object SCollection) */
  class CollCoster[T](obj: RCosted[Coll[T]], method: SMethod, args: Seq[RCosted[_]]) extends Coster[Coll[T]](obj, method, args) {
    import Coll._
    implicit val eT = obj.elem.eVal.eItem

    def indices(): RCostedColl[Int] =
      knownLengthCollPropertyAccess(_.indices, asSizeColl(obj.size).sizes.length)

    def map[B](_f: RCosted[T => B]): RCosted[Coll[B]] = {
      val xs = asCostedColl(obj)
      val f = asCostedFunc[T,B](_f)
      val calcF = f.sliceCalc
      val costF = f.sliceCost
      val sizeF = f.sliceSize
      val vals = xs.values.map(calcF)
      implicit val eT = xs.elem.eItem
      implicit val eB = f.elem.eVal.eRange
      val costs = xs.costs.zip(xs.sizes).map(costF)
      val sizes = if (eB.isConstantSize) {
        colBuilder.replicate(xs.sizes.length, constantTypeSize(eB): RSize[B])
      } else {
        xs.sizes.map(sizeF)
      }
      RCCostedColl(vals, costs, sizes, opCost(vals, costOfArgs, costOf(method)))
    }

    def flatMap[B](fC: RCosted[T => Coll[B]]): RCostedColl[B] = {
      val f = fC.value
      f match {
        // Pattern: xs.flatMap(x => x.property)
        case Def(Lambda(l,_,_,Def(mc @ MethodCall(x, m, Nil, _)))) if x == l.x =>
          val sObj = asSizeColl(obj.size)
          val sizeF = asCostedFunc[T, Coll[B]](fC).sliceSize
          val sizes: RColl[Size[B]] = sObj.sizes.flatMap(fun { s: RSize[T] =>
            asSizeColl(sizeF(s)).sizes
          })
          val values = obj.value.flatMap(f)
          val costs = colBuilder.replicate(sizes.length, 0)
          RCCostedColl(values, costs, sizes, opCost(values, costOfArgs, costOf(method)))
        case _ =>
          !!!(s"Unsupported lambda in flatMap: allowed usage `xs.flatMap(x => x.property)`")
      }
    }

    def indexOf(elem: RCosted[T], from: RCosted[Int]): RCosted[Int] = {
      val value = obj.value.indexOf(elem.value, from.value)
      val c = opCost(value, costOfArgs, perKbCostOf(method, obj.size.dataSize))
      RCCostedPrim(value, c, SizeInt)
    }

    def segmentLength(p: RCosted[T => Boolean], from: RCosted[Int]): RCosted[Int] = {
//      val pCost: Rep[((Int, Size[A])) => Int] = asCostedFunc(p).func.sliceCost
      // TODO costing rule should be more accurate
      val value = obj.value.segmentLength(p.value, from.value)
      val c = opCost(value, costOfArgs, costOf(method))
      RCCostedPrim(value, c, SizeInt)
    }

    def indexWhere(p: RCosted[T => Boolean], from: RCosted[Int]): RCosted[Int] = {
      // TODO costing rule should be more accurate
      val value = obj.value.indexWhere(p.value, from.value)
      val c = opCost(value, costOfArgs, costOf(method))
      RCCostedPrim(value, c, SizeInt)
    }

    def lastIndexWhere(p: RCosted[T => Boolean], end: RCosted[Int]): RCosted[Int] = {
      // TODO costing rule should be more accurate
      val value = obj.value.lastIndexWhere(p.value, end.value)
      val c = opCost(value, costOfArgs, costOf(method))
      RCCostedPrim(value, c, SizeInt)
    }

    def zip[B](ys: RCosted[Coll[B]]): RCosted[Coll[(T, B)]] = {
      implicit val eB = ys.elem.eVal.eItem
      val values = obj.value.zip(ys.value)
      val xsC = asCostedColl(obj)
      val ysC = asCostedColl(ys)
      // TODO optimize: it make sence to add more high level operations to avoid building large graphs
      val costs = xsC.costs.zip(ysC.costs).map(fun { in: Rep[(Int,Int)] => in._1 + in._2 })
      val sizes = xsC.sizes.zip(ysC.sizes).map(fun { in: Rep[(Size[T],Size[B])] => RCSizePair(in._1, in._2): RSize[(T,B)] })
      val c = opCost(values, costOfArgs, costOf(method))
      RCCostedColl(values, costs, sizes, c)
    }

    def partition(pred: RCosted[T => Boolean]): RCosted[(Coll[T], Coll[T])] = {
      // TODO costing rule should be more accurate
      val xsC = asCostedColl(obj)
      val Pair(lvalues, rvalues) = xsC.value.partition(pred.value)
      val costs = xsC.costs
      val sizes = xsC.sizes
      val l = RCCostedColl(lvalues, costs, sizes, CostTable.newCollValueCost)
      val r = RCCostedColl(rvalues, costs, sizes, CostTable.newCollValueCost)
      val c = opCost(Pair(l, r), costOfArgs, costOf(method))
      RCCostedPair(l, r, c)
    }

    def patch(from: RCosted[Int], patch: RCosted[Coll[T]], replaced: RCosted[Int]): RCosted[Coll[T]] = {
      val xsC = asCostedColl(obj)
      val patchC = asCostedColl(patch)
      val values = xsC.value.patch(from.value, patch.value, replaced.value)
      val sizes = xsC.sizes.append(patchC.sizes)
      val costs = xsC.costs.append(patchC.costs)
      val c = opCost(values, costOfArgs, costOf(method)) // TODO costing rule should be more accurate
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
      val c = opCost(v, costOfArgs, perKbCostOf(method, values.size.dataSize))  // TODO costing rule should be more accurate with sizes
      RCCostedColl(v, xsC.costs, xsC.sizes, c)
    }

    def filter(_f: RCosted[T => Boolean]): RCosted[Coll[T]] = {
      val xs = asCostedColl(obj)
      val f = asCostedFunc[T,Boolean](_f)
      val calcF = f.sliceCalc
      val costF = f.sliceCost
      val vals = xs.values.filter(calcF)
      val costs = xs.costs.zip(xs.sizes).map(costF)
      val zeros = colBuilder.replicate(xs.costs.length, 0)
      RCCostedColl(vals, zeros, xs.sizes, opCost(vals, costOfArgs, costOf(method) + costs.sum(intPlusMonoid)))
    }
  }

  object CollCoster extends CostingHandler[Coll[Any]]((obj, m, args) => new CollCoster[Any](obj, m, args))

  /** Costing rules for SGlobal methods */
  class SigmaDslBuilderCoster(obj: RCosted[SigmaDslBuilder], method: SMethod, args: Seq[RCosted[_]]) extends Coster[SigmaDslBuilder](obj, method, args){

    def groupGenerator() = groupElementPropertyAccess(_.groupGenerator)
  }

  object SigmaDslBuilderCoster extends CostingHandler[SigmaDslBuilder]((obj, m, args) => new SigmaDslBuilderCoster(obj, m, args))

}
