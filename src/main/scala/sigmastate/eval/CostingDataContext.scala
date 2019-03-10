package sigmastate.eval

import java.math.BigInteger

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.ErgoBox
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADDigest, ADKey, SerializedAdProof, ADValue}
import sigmastate.SCollection.SByteArray
import sigmastate.{TrivialProp, _}
import sigmastate.Values.{Constant, SValue, ConstantNode, Value, IntConstant, ErgoTree, SigmaBoolean}
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.{CryptoConstants, Interpreter}
import special.collection.{Builder, CCostedBuilder, CollType, CostedBuilder, Coll}
import special.sigma._
import special.sigma.Extensions._

import scala.util.{Success, Failure}
import scalan.RType
import scorex.crypto.hash.{Sha256, Digest32, Blake2b256}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.ProveDHTuple
import sigmastate.interpreter.Interpreter.emptyEnv
import sigmastate.lang.Terms.OperationId
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer

import scala.reflect.ClassTag

trait WrapperOf[T] {
  def wrappedValue: T
}

case class CBigInt(override val wrappedValue: BigInteger) extends TestBigInt(wrappedValue) with WrapperOf[BigInteger] {
  override val dsl = CostingSigmaDslBuilder
}

case class CGroupElement(override val wrappedValue: ECPoint) extends TestGroupElement(wrappedValue) with WrapperOf[ECPoint] {
  override val dsl = CostingSigmaDslBuilder
}

case class CSigmaProp(sigmaTree: SigmaBoolean) extends SigmaProp with WrapperOf[SigmaBoolean] {
  override def wrappedValue: SigmaBoolean = sigmaTree
  override def isValid: Boolean = sigmaTree match {
    case p: TrivialProp => p.condition
    case _ => sys.error(s"Method CostingSigmaProp.isValid is not defined for $sigmaTree")
  }

  override def propBytes: Coll[Byte] = {
    // in order to have comparisons like  `box.propositionBytes == pk.propBytes` we need to make sure
    // the same serialization method is used in both cases
    val ergoTree = ErgoTree.fromSigmaBoolean(sigmaTree)
    val bytes = DefaultSerializer.serializeErgoTree(ergoTree)
    Builder.DefaultCollBuilder.fromArray(bytes)
  }

  override def &&(other: SigmaProp): SigmaProp = other match {
    case other: CSigmaProp =>
      CSigmaProp(CAND.normalized(Seq(sigmaTree, other.sigmaTree)))
  }

  override def &&(other: Boolean): SigmaProp =
    CSigmaProp(CAND.normalized(Seq(sigmaTree, TrivialProp(other))))

  override def ||(other: SigmaProp): SigmaProp = other match {
    case other: CSigmaProp =>
      CSigmaProp(COR.normalized(Seq(sigmaTree, other.sigmaTree)))
  }

  override def ||(other: Boolean): SigmaProp =
    CSigmaProp(COR.normalized(Seq(sigmaTree, TrivialProp(other))))

  override def toString: String = s"SigmaProp(${wrappedValue.showToString})"
}

case class CAvlTree(treeData: AvlTreeData) extends AvlTree with WrapperOf[AvlTreeData] {
  val builder = CostingSigmaDslBuilder
  val Colls = builder.Colls

  override def wrappedValue: AvlTreeData = treeData

  def startingDigest: Coll[Byte] = Colls.fromArray(treeData.digest)

  def keyLength: Int = treeData.keyLength

  def enabledOperations = treeData.treeFlags.serializeToByte

  override def isInsertAllowed: Boolean = treeData.treeFlags.insertAllowed

  override def isUpdateAllowed: Boolean = treeData.treeFlags.updateAllowed

  override def isRemoveAllowed: Boolean = treeData.treeFlags.removeAllowed

  override def updateOperations(newOperations: Byte): AvlTree = {
    val td = treeData.copy(treeFlags = AvlTreeFlags(newOperations))
    this.copy(treeData = td)
  }

  def valueLengthOpt: Option[Int] = treeData.valueLengthOpt

  def cost: Int = 1

  def dataSize: Long = SAvlTree.dataSize(treeData.asInstanceOf[SType#WrappedType])

  override def digest: Coll[Byte] = Colls.fromArray(treeData.digest)

  def updateDigest(newDigest: Coll[Byte]): AvlTree = {
    val td = treeData.copy(digest = ADDigest @@ newDigest.toArray)
    this.copy(treeData = td)
  }

  private def createVerifier(proof: Coll[Byte]): BatchAVLVerifier[Digest32, Blake2b256.type] = {
    val adProof = SerializedAdProof @@ proof.toArray
    val bv = new BatchAVLVerifier[Digest32, Blake2b256.type](
      treeData.digest, adProof,
      treeData.keyLength, treeData.valueLengthOpt)
    bv
  }

  override def contains(key: Coll[Byte], proof: Coll[Byte]): Boolean = {
    val keyBytes = key.toArray
    val bv = createVerifier(proof)
    bv.performOneOperation(Lookup(ADKey @@ keyBytes)) match {
      case Failure(_) => false
      case Success(r) => r match {
        case Some(_) => true
        case _ => false
      }
    }
  }

  override def get(key: Coll[Byte], proof: Coll[Byte]): Option[Coll[Byte]] = {
    val keyBytes = key.toArray
    val bv = createVerifier(proof)
    bv.performOneOperation(Lookup(ADKey @@ keyBytes)) match {
      case Failure(_) => Interpreter.error(s"Tree proof is incorrect $treeData")
      case Success(r) => r match {
        case Some(v) => Some(Colls.fromArray(v))
        case _ => None
      }
    }
  }

  override def getMany(keys: Coll[Coll[Byte]], proof: Coll[Byte]): Coll[Option[Coll[Byte]]] = {
    val bv = createVerifier(proof)
    keys.map {key =>
      bv.performOneOperation(Lookup(ADKey @@ key.toArray)) match {
        case Failure(_) => Interpreter.error(s"Tree proof is incorrect $treeData")
        case Success(r) => r match {
          case Some(v) => Some(Colls.fromArray(v))
          case _ => None
        }
      }
    }
  }

  override def insert(operations: Coll[(Coll[Byte], Coll[Byte])], proof: Coll[Byte]): Option[AvlTree] = {
    if (!isInsertAllowed) {
      None
    } else {
      val bv = createVerifier(proof)
      operations.forall { case (key, value) =>
        bv.performOneOperation(Insert(ADKey @@ key.toArray, ADValue @@ value.toArray)).isSuccess
      }
      bv.digest match {
        case Some(d) => Some(updateDigest(Colls.fromArray(d)))
        case _ => None
      }
    }
  }

  override def update(operations: Coll[(Coll[Byte], Coll[Byte])], proof: Coll[Byte]): Option[AvlTree] = {
    if (!isUpdateAllowed) {
      None
    } else {
      val bv = createVerifier(proof)
      operations.forall { case (key, value) =>
        bv.performOneOperation(Update(ADKey @@ key.toArray, ADValue @@ value.toArray)).isSuccess
      }
      bv.digest match {
        case Some(d) => Some(updateDigest(Colls.fromArray(d)))
        case _ => None
      }
    }
  }

  override def remove(operations: Coll[Coll[Byte]], proof: Coll[Byte]): Option[AvlTree] = {
    if (!isRemoveAllowed) {
      None
    } else {
      val keysToRemove = operations.toArray.map(_.toArray)
      val bv = createVerifier(proof)
      keysToRemove.foreach(key => bv.performOneOperation(Remove(ADKey @@ key)))
      bv.digest match {
        case Some(v) => Some(updateDigest(Colls.fromArray(v)))
        case _ => None
      }
    }
  }
}

import sigmastate.eval.CostingBox._

case class CostingBox(val IR: Evaluation,
                 isCost: Boolean,
                 val ebox: ErgoBox)
  extends TestBox(
    colBytes(ebox.id)(IR),
    ebox.value,
    colBytes(ebox.bytes)(IR),
    colBytes(ebox.bytesWithNoRef)(IR),
    colBytes(ebox.propositionBytes)(IR),
    regs(ebox, isCost)(IR)
  ) with WrapperOf[ErgoBox]
{
  override val builder = new CostingSigmaDslBuilder()

  override def wrappedValue: ErgoBox = ebox

  override def getReg[T](i: Int)(implicit tT: RType[T]): Option[T] =
    if (isCost) {
      val optV =
        if (i < 0 || i >= registers.length) None
        else {
          val value = registers(i)
          if (value != null ) {
            // once the value is not null it should be of the right type
            value match {
              case value: TestValue[_] if value.value != null =>
                Some(value.value.asInstanceOf[T])
              case _ =>
                None
            }
          } else None
        }

      optV.orElse {
        val tpe = Evaluation.rtypeToSType(tT)
        val default = builder.Costing.defaultValue(tT).asInstanceOf[SType#WrappedType]
        Some(Constant[SType](default, tpe).asInstanceOf[T])
      }
    } else
      super.getReg(i)(tT)

  override def creationInfo: (Int, Coll[Byte]) = {
    this.getReg[(Int, Coll[Byte])](3).get.asInstanceOf[Any] match {
      case info: Tuple2[Int, Coll[Byte]] @unchecked => info
      case ConstantNode(arr: Array[Any], STuple(IndexedSeq(SInt, SByteArray))) if arr.length == 2 =>
        (arr(0).asInstanceOf[Int], builder.Colls.fromArray(arr(1).asInstanceOf[Array[Byte]]))
      case v =>
        sys.error(s"Invalid value $v of creationInfo register R3")
    }

  }
}

object CostingBox {
  import Evaluation._
  import sigmastate.SType._
  def colBytes(b: Array[Byte])(implicit IR: Evaluation): Coll[Byte] = IR.sigmaDslBuilderValue.Colls.fromArray(b)

  def regs(ebox: ErgoBox, isCost: Boolean)(implicit IR: Evaluation): Coll[AnyValue] = {
    val res = new Array[AnyValue](ErgoBox.maxRegisters)

    def checkNotYetDefined(id: Int, newValue: SValue) =
      require(res(id) == null, s"register $id is defined more then once: previous value ${res(id)}, new value $newValue")

    for ((k, v: Value[t]) <- ebox.additionalRegisters) {
      checkNotYetDefined(k.number, v)
      val dslData = toDslData(v, v.tpe, isCost)
      res(k.number) = toAnyValue(dslData.asWrappedType)(stypeToRType(v.tpe))
    }

    for (r <- ErgoBox.mandatoryRegisters) {
      val regId = r.number
      val v = ebox.get(r).get
      checkNotYetDefined(regId, v)
      val dslData = Evaluation.toDslData(v, v.tpe, isCost)
      res(regId) = toAnyValue(dslData.asWrappedType)(stypeToRType(v.tpe))
    }
    IR.sigmaDslBuilderValue.Colls.fromArray(res)
  }

}

/** This class represents context variable and register value of a functional type A => B.
  * When variable or register is accessed using `getVar[A => B](id).get` or
  * `box.getReg[A => B].get an instance of this class is returned.
  *
  * It internally transforms a given `tree` into executable function.
  * This it similar to what happens during validation of propositions in the input boxes:
  * - size check of underlying ErgoTree against limits
  * - construction of `calcF` and `costF` graphs, both are stored together with resulting function.
  * - check the types of `calcF` graph to be compatible with expected types A and B
  * If anything goes wrong, this operation fails and if it is used in the script, the script also fails.
  *
  * When f is obtained as `val f = getVar[Int => Int](id).get` then any application `f(x)` involves size estimation
  * using underlying `costF(x)`.
  */
//case class CFunc[A,B](context: sigmastate.interpreter.Context, tree: SValue)
//    (implicit tDom: RType[A], tRange: RType[B], IR: IRContext) extends (A => B) {
//  import CFunc._
//
//  private val compiled = {
//    import IR._
//    val IR.Pair(calcF, costF) = IR.doCosting(emptyEnv, tree)
//
//    val eDom = asElem[Any](IR.rtypeToElem(tDom))
//    val eRange = asElem[Any](IR.rtypeToElem(tRange))
//
//    IR.verifyCalcFunc[Any => Any](asRep[Context => (Any => Any)](calcF), IR.funcElement(eDom, eRange))
////    IR.verifyCostFunc(costF).fold(t => throw t, x => x)
////    IR.verifyIsProven(calcF).fold(t => throw t, x => x)
//
//    // check cost
////    val costingCtx = context.toSigmaContext(IR, isCost = true)
////    val costFun = IR.compile[SInt.type](IR.getDataEnv, costF)
////    val IntConstant(estimatedCost) = costFun(costingCtx)
////    if (estimatedCost > maxCost) {
////      throw new Error(s"Estimated expression complexity $estimatedCost exceeds the limit $maxCost in $tree")
////    }
//    // check calc
//    val calcCtx = context.toSigmaContext(IR, isCost = false)
//    val valueFun = IR.compile[SFunc](IR.getDataEnv, asRep[Context => SFunc#WrappedType](calcF))
//    val res = valueFun(calcCtx) match {
//      case Constant(f, fTpe: SFunc) => f
//      case v => v
//    }
//    res.asInstanceOf[A => B]
//  }
//
//  override def apply(x: A): B = compiled(x)
//}
object CFunc {
  /** The cost of creating resulting function but not its execution.
    * Thus it is expected to be small. It can be increased if useful cases are found
    * such that `tree` should contains heavy operations. */
  val maxCost = 1000
}

case class CPreHeader(
  version: Byte,
  parentId: Coll[Byte],
  timestamp: Long,
  nBits: Long,
  height: Int,
  minerPk: GroupElement,
  votes: Coll[Byte],
) extends PreHeader {}

case class CHeader(
  id: Coll[Byte],
  version: Byte,
  parentId: Coll[Byte],
  ADProofsRoot: Coll[Byte],
  stateRoot: AvlTree,
  transactionsRoot: Coll[Byte],
  timestamp: Long,
  nBits: Long,
  height: Int,
  extensionRoot: Coll[Byte],
  minerPk: GroupElement,
  powOnetimePk: GroupElement,
  powNonce: Coll[Byte],
  powDistance: BigInt,
  votes: Coll[Byte],
) extends Header {
}

class CCostModel extends CostModel {
  private def costOf(opName: String, opType: SFunc): Int = {
    val operId = OperationId(opName, opType)
    costOf(operId)
  }
  @inline private def costOf(operId: OperationId): Int = {
    val cost = sigmastate.utxo.CostTable.DefaultCosts(operId)
    cost
  }

  def AccessBox: Int = costOf("AccessBox", SFunc(SContext, SBox))

  def AccessAvlTree: Int = costOf("AccessAvlTree", SFunc(SContext, SAvlTree))

  def GetVar: Int = costOf("GetVar", SFunc(IndexedSeq(SContext, SByte), SOption(SOption.tT)))

  def DeserializeVar: Int = costOf("DeserializeVar", SFunc(IndexedSeq(SContext, SByte), SOption(SOption.tT)))

  def GetRegister: Int = costOf("GetRegister", SFunc(IndexedSeq(SBox, SByte), SOption(SOption.tT)))

  def DeserializeRegister: Int  = costOf("DeserializeRegister", SFunc(IndexedSeq(SBox, SByte), SOption(SOption.tT)))

  def SelectField: Int      = costOf("SelectField", SFunc(IndexedSeq(), SUnit))

  def CollectionConst: Int  = costOf("Const", SFunc(IndexedSeq(), SCollection(STypeIdent("IV"))))

  def AccessKiloByteOfData: Int  = costOf("AccessKiloByteOfData", SFunc(IndexedSeq(), SUnit))

  def dataSize[T](x: T)(implicit cT: ClassTag[T]): Long = SigmaPredef.dataSize(x)

  def PubKeySize: Long = CryptoConstants.EncodedGroupElementLength
}

class CostingSigmaDslBuilder extends TestSigmaDslBuilder { dsl =>
  override val Costing: CostedBuilder = new CCostedBuilder {
    import RType._
    override def defaultValue[T](valueType: RType[T]): T = (valueType match {
      case BooleanType  => false
      case ByteType => 0.toByte
      case ShortType => 0.toShort
      case IntType  => 0
      case LongType => 0L
      case StringType => ""
      case CharType => 0.toChar
      case FloatType => 0.0f
      case DoubleType => 0.0d
      case p: PairType[a, b] => (defaultValue(p.tFst), defaultValue(p.tSnd))
      case col: CollType[a] => dsl.Colls.emptyColl(col.tItem)
      case tup: TupleType => tup.items.map(t => defaultValue(t))
      case SType.AvlTreeDataRType => AvlTreeData.dummy
      case AvlTreeRType => CAvlTree(AvlTreeData.dummy)

      case SType.SigmaBooleanRType => TrivialProp.FalseProp
      case SigmaPropRType => sigmaProp(false)

      case ECPointRType => CryptoConstants.dlogGroup.generator
      case GroupElementRType => groupGenerator

      case _ => sys.error(s"Cannot create defaultValue($valueType)")
    }).asInstanceOf[T]
  }

  override def CostModel: CostModel = new CCostModel

  override def BigInt(n: BigInteger): BigInt = new CBigInt(n)

  override def GroupElement(p: ECPoint): GroupElement = new CGroupElement(p)

  def SigmaProp(sigmaTree: SigmaBoolean): SigmaProp = new CSigmaProp(sigmaTree)

  /** Extract `sigmastate.Values.SigmaBoolean` from DSL's `SigmaProp` type. */
  def toSigmaBoolean(p: SigmaProp): SigmaBoolean = p.asInstanceOf[CSigmaProp].sigmaTree

  /** Extract `sigmastate.AvlTreeData` from DSL's `AvlTree` type. */
  def toAvlTreeData(p: AvlTree): AvlTreeData = p.asInstanceOf[CAvlTree].treeData

  override def avlTree(operationFlags: Byte, digest: Coll[Byte], keyLength: Int, valueLengthOpt: Option[Int]): AvlTree = {
    val treeData = AvlTreeData(ADDigest @@ digest.toArray, AvlTreeFlags(operationFlags), keyLength, valueLengthOpt)
    CAvlTree(treeData)
  }

  private def toSigmaTrees(props: Array[SigmaProp]): Array[SigmaBoolean] = {
    props.map { case csp: CSigmaProp => csp.sigmaTree }
  }

  @inline private def toEcPointType(ge: GroupElement): EcPointType =
    toECPoint(ge).asInstanceOf[EcPointType]

  override def atLeast(bound: Int, props: Coll[SigmaProp]): SigmaProp = {
    if (props.length > AtLeast.MaxChildrenCount)
      throw new IllegalArgumentException(s"Expected input elements count should not exceed ${AtLeast.MaxChildrenCount}, actual: ${props.length}")
    val sigmaTrees = toSigmaTrees(props.toArray)
    val tree = AtLeast.reduce(bound, sigmaTrees)
    CSigmaProp(tree)
  }

  override def allZK(props: Coll[SigmaProp]): SigmaProp = {
    val sigmaTrees = toSigmaTrees(props.toArray)
    val tree = CAND.normalized(sigmaTrees)
    CSigmaProp(tree)
  }

  override def anyZK(props: Coll[SigmaProp]): SigmaProp = {
    val sigmaTrees = toSigmaTrees(props.toArray)
    val tree = COR.normalized(sigmaTrees)
    CSigmaProp(tree)
  }

  override def sigmaProp(b: Boolean): SigmaProp = {
    CSigmaProp(TrivialProp(b))
  }

  override def blake2b256(bytes: Coll[Byte]): Coll[Byte] = {
    val h = Blake2b256.hash(bytes.toArray)
    Colls.fromArray(h)
  }

  override def sha256(bytes: Coll[Byte]): Coll[Byte] = {
    val h = Sha256.hash(bytes.toArray)
    Colls.fromArray(h)
  }

  override def proveDlog(ge: GroupElement): SigmaProp =
    CSigmaProp(ProveDlog(toECPoint(ge).asInstanceOf[EcPointType]))

  override def proveDHTuple(g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement): SigmaProp = {
    val dht = ProveDHTuple(toEcPointType(g), toEcPointType(h), toEcPointType(u), toEcPointType(v))
    CSigmaProp(dht)
  }

  override def groupGenerator: GroupElement = {
    this.GroupElement(CryptoConstants.dlogGroup.generator)
  }

  override def substConstants[T](scriptBytes: Coll[Byte],
      positions: Coll[Int],
      newValues: Coll[T])
      (implicit cT: RType[T]): Coll[Byte] = {
    val typedNewVals = newValues.toArray.map(_.asInstanceOf[Value[SType]])
    val res = SubstConstants.eval(scriptBytes.toArray, positions.toArray, typedNewVals)
    Colls.fromArray(res)
  }

  override def decodePoint(encoded: Coll[Byte]): GroupElement = {
    this.GroupElement(CryptoConstants.dlogGroup.curve.decodePoint(encoded.toArray))
  }
}

object CostingSigmaDslBuilder extends CostingSigmaDslBuilder

case class CostingDataContext(
    _dataInputs: Coll[Box],
    override val headers: Coll[Header],
    override val preHeader: PreHeader,
    inputs: Coll[Box],
    outputs: Coll[Box],
    height: Int,
    selfBox: Box,
    lastBlockUtxoRootHash: AvlTree,
    _minerPubKey: Coll[Byte],
    vars: Coll[AnyValue],
    var isCost: Boolean)
    extends Context
{
  @inline def builder: SigmaDslBuilder = CostingSigmaDslBuilder
  @inline def HEIGHT: Int = height
  @inline def SELF: Box   = selfBox
  @inline def dataInputs: Coll[Box] = _dataInputs
  @inline def INPUTS = inputs
  @inline def OUTPUTS = outputs
  @inline def LastBlockUtxoRootHash = lastBlockUtxoRootHash
  @inline def minerPubKey = _minerPubKey


  def findSelfBoxIndex: Int = {
    var i = 0
    while (i < inputs.length) {
      if (inputs(i) eq selfBox) return i
      i += 1
    }
    -1
  }

  override val selfBoxIndex: Int = findSelfBoxIndex

  override def getVar[T](id: Byte)(implicit tT: RType[T]): Option[T] = {
    if (isCost) {
      //      implicit val tag: ClassTag[T] = cT.classTag
      val optV =
        if (id < 0 || id >= vars.length) None
        else {
          val value = vars(id)
          if (value != null ) {
            // once the value is not null it should be of the right type
            value match {
              case value: TestValue[_] if value.value != null =>
                Some(value.value.asInstanceOf[T])
              case _ => None
            }
          } else None
        }
      optV.orElse {
        val tpe = Evaluation.rtypeToSType(tT)
        val default = builder.Costing.defaultValue(tT).asInstanceOf[SType#WrappedType]
        Some(Constant[SType](default, tpe).asInstanceOf[T])
      }
    } else {
      implicit val tag: ClassTag[T] = tT.classTag
      if (id < 0 || id >= vars.length) return None
      val value = vars(id)
      if (value != null ) {
        // once the value is not null it should be of the right type
        value match {
          case value: TestValue[_] if value.value != null && value.tA == tT =>
            Some(value.value.asInstanceOf[T])
          case _ =>
            throw new InvalidType(s"Cannot getVar[${tT.name}]($id): invalid type of value $value at id=$id")
        }
      } else None
    }
  }
}
