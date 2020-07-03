package sigmastate.eval

import java.math.BigInteger
import java.util

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.{ErgoBox, SigmaConstants}
import org.ergoplatform.validation.ValidationRules
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADDigest, ADKey, ADValue, SerializedAdProof}
import sigmastate.SCollection.SByteArray
import sigmastate.{TrivialProp, _}
import sigmastate.Values.{Constant, ConstantNode, ErgoTree, EvaluatedValue, SValue, SigmaBoolean, Value}
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.{CryptoConstants, Interpreter}
import special.collection.{CCostedBuilder, CSizeOption, Coll, CollType, CostedBuilder, Size, SizeColl, SizeOption}
import special.sigma.{Box, _}
import sigmastate.eval.Extensions._
import spire.syntax.all.cfor

import scala.util.{Failure, Success}
import scalan.RType
import scorex.crypto.hash.{Blake2b256, Digest32, Sha256}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.ProveDHTuple
import sigmastate.lang.Terms.OperationId
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.serialization.{GroupElementSerializer, SigmaSerializer}
import special.Types.TupleType

import scala.reflect.ClassTag

trait WrapperOf[T] {
  def wrappedValue: T
}

case class CBigInt(override val wrappedValue: BigInteger) extends TestBigInt(wrappedValue) with WrapperOf[BigInteger] {
  override val dsl = CostingSigmaDslBuilder
}

case class CGroupElement(override val wrappedValue: EcPointType) extends TestGroupElement(wrappedValue) with WrapperOf[ECPoint] {
  override val dsl = CostingSigmaDslBuilder

  override def getEncoded: Coll[Byte] = dsl.Colls.fromArray(GroupElementSerializer.toBytes(wrappedValue))

}

case class CSigmaProp(sigmaTree: SigmaBoolean) extends SigmaProp with WrapperOf[SigmaBoolean] {
  override def wrappedValue: SigmaBoolean = sigmaTree

  // TODO refactor: remove this (it shouldn't be used in interpreter)
  override def isValid: Boolean = sigmaTree match {
    case p: TrivialProp => p.condition
    case _ => sys.error(s"Method CostingSigmaProp.isValid is not defined for $sigmaTree")
  }

  override def propBytes: Coll[Byte] = {
    // in order to have comparisons like  `box.propositionBytes == pk.propBytes` we need to make sure
    // the same serialization method is used in both cases
    val ergoTree = ErgoTree.fromSigmaBoolean(sigmaTree)
    val bytes = DefaultSerializer.serializeErgoTree(ergoTree)
    Colls.fromArray(bytes)
  }

  override def &&(other: SigmaProp): SigmaProp = other match {
    case other: CSigmaProp =>
      CSigmaProp(CAND.normalized(Array(sigmaTree, other.sigmaTree)))
  }

  // TODO refactor: remove this (it shouldn't be used in interpreter)
  override def &&(other: Boolean): SigmaProp =
    CSigmaProp(CAND.normalized(Array(sigmaTree, TrivialProp(other))))

  override def ||(other: SigmaProp): SigmaProp = other match {
    case other: CSigmaProp =>
      CSigmaProp(COR.normalized(Array(sigmaTree, other.sigmaTree)))
  }

  // TODO refactor: remove this (it shouldn't be used in interpreter)
  override def ||(other: Boolean): SigmaProp =
    CSigmaProp(COR.normalized(Array(sigmaTree, TrivialProp(other))))

  override def toString: String = s"SigmaProp(${wrappedValue.showToString})"
}

case class CAvlTree(treeData: AvlTreeData) extends AvlTree with WrapperOf[AvlTreeData] {
  val builder = CostingSigmaDslBuilder
  val Colls = builder.Colls

  override def wrappedValue: AvlTreeData = treeData

  override def keyLength: Int = treeData.keyLength

  override def enabledOperations = treeData.treeFlags.serializeToByte

  override def isInsertAllowed: Boolean = treeData.treeFlags.insertAllowed

  override def isUpdateAllowed: Boolean = treeData.treeFlags.updateAllowed

  override def isRemoveAllowed: Boolean = treeData.treeFlags.removeAllowed

  override def updateOperations(newOperations: Byte): AvlTree = {
    val td = treeData.copy(treeFlags = AvlTreeFlags(newOperations))
    this.copy(treeData = td)
  }

  override def valueLengthOpt: Option[Int] = treeData.valueLengthOpt

  def cost: Int = 1

  def dataSize: Long = SAvlTree.dataSize(treeData.asInstanceOf[SType#WrappedType])

  override def digest: Coll[Byte] = Colls.fromArray(treeData.digest)

  override def updateDigest(newDigest: Coll[Byte]): AvlTree = {
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
      case Success(r) => r match {
        case Some(_) => true
        case _ => false
      }
      case Failure(_) => false
    }
  }

  override def get(key: Coll[Byte], proof: Coll[Byte]): Option[Coll[Byte]] = {
    val keyBytes = key.toArray
    val bv = createVerifier(proof)
    bv.performOneOperation(Lookup(ADKey @@ keyBytes)) match {
      case Success(r) => r match {
        case Some(v) => Some(Colls.fromArray(v))
        case _ => None
      }
      case Failure(_) => Interpreter.error(s"Tree proof is incorrect $treeData")
    }
  }

  override def getMany(keys: Coll[Coll[Byte]], proof: Coll[Byte]): Coll[Option[Coll[Byte]]] = {
    val bv = createVerifier(proof)
    keys.map { key =>
      bv.performOneOperation(Lookup(ADKey @@ key.toArray)) match {
        case Success(r) => r match {
          case Some(v) => Some(Colls.fromArray(v))
          case _ => None
        }
        case Failure(_) => Interpreter.error(s"Tree proof is incorrect $treeData")
      }
    }
  }

  override def insert(entries: Coll[(Coll[Byte], Coll[Byte])], proof: Coll[Byte]): Option[AvlTree] = {
    if (!isInsertAllowed) {
      None
    } else {
      val bv = createVerifier(proof)
      entries.forall { case (key, value) =>
        val insertRes = bv.performOneOperation(Insert(ADKey @@ key.toArray, ADValue @@ value.toArray))
        if (insertRes.isFailure) {
          Interpreter.error(s"Incorrect insert for $treeData (key: $key, value: $value, digest: $digest): ${insertRes.failed.get}}")
        }
        insertRes.isSuccess
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
      val bv = createVerifier(proof)
      cfor(0)(_ < operations.length, _ + 1) { i =>
        val key = operations(i).toArray
        bv.performOneOperation(Remove(ADKey @@ key))
      }
      bv.digest match {
        case Some(v) => Some(updateDigest(Colls.fromArray(v)))
        case _ => None
      }
    }
  }
}

import sigmastate.eval.CostingBox._

class EvalSizeBox(
    propositionBytes: Size[Coll[Byte]],
    bytes: Size[Coll[Byte]],
    bytesWithoutRef: Size[Coll[Byte]],
    registers: Size[Coll[Option[AnyValue]]],
    tokens: Size[Coll[(Coll[Byte], Long)]]
) extends CSizeBox(propositionBytes, bytes, bytesWithoutRef, registers, tokens) {
  override def getReg[T](id: Byte)(implicit tT: RType[T]): Size[Option[T]] = {
    val varSize = registers.asInstanceOf[SizeColl[Option[AnyValue]]].sizes(id.toInt)
    val foundSize = varSize.asInstanceOf[SizeOption[AnyValue]].sizeOpt
    val regSize = foundSize match {
      case Some(varSize: SizeAnyValue) =>
        assert(varSize.tVal == tT, s"Unexpected register type found at register #$id: ${varSize.tVal}, expected $tT")
        val regSize = varSize.valueSize.asInstanceOf[Size[T]]
        regSize
      case _ =>
        val z = Zero.typeToZero(tT)
        val zeroVal = z.zero
        val sized = Sized.typeToSized(tT)
        sized.size(zeroVal)
    }
    new CSizeOption[T](Some(regSize))
  }
}
class EvalSizeBuilder extends CSizeBuilder {
  override def mkSizeBox(propositionBytes: Size[Coll[Byte]], bytes: Size[Coll[Byte]],
      bytesWithoutRef: Size[Coll[Byte]], registers: Size[Coll[Option[AnyValue]]],
      tokens: Size[Coll[(Coll[Byte], Long)]]): SizeBox = {
    new EvalSizeBox(propositionBytes, bytes, bytesWithoutRef, registers, tokens)
  }
}

case class CostingBox(isCost: Boolean, val ebox: ErgoBox) extends Box with WrapperOf[ErgoBox] {
  val builder = CostingSigmaDslBuilder

  val value = ebox.value
  lazy val id: Coll[Byte] = Colls.fromArray(ebox.id)
  lazy val bytes: Coll[Byte] = Colls.fromArray(ebox.bytes)
  lazy val bytesWithoutRef: Coll[Byte] = Colls.fromArray(ebox.bytesWithNoRef)
  lazy val propositionBytes: Coll[Byte] = Colls.fromArray(ebox.propositionBytes)
  lazy val registers: Coll[AnyValue] = regs(ebox, isCost)

  override def wrappedValue: ErgoBox = ebox

  override def getReg[T](i: Int)(implicit tT: RType[T]): Option[T] =
    if (isCost) {
      val optV =
        if (i < 0 || i >= registers.length) None
        else {
          val value = registers(i)
          if (value != null) {
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
    } else {
      if (i < 0 || i >= registers.length) return None
      val value = registers(i)
      if (value != null ) {
        // once the value is not null it should be of the right type
        value match {
          case value: TestValue[_] if value.value != null && value.tA == tT =>
            Some(value.value.asInstanceOf[T])
          case _ =>
            throw new InvalidType(s"Cannot getReg[${tT.name}]($i): invalid type of value $value at id=$i")
        }
      } else None
    }


  override def creationInfo: (Int, Coll[Byte]) = {
    this.getReg[(Int, Coll[Byte])](3).get.asInstanceOf[Any] match {
      case info: Tuple2[Int, Coll[Byte]]@unchecked => info
      case ConstantNode(arr: Array[Any], STuple(IndexedSeq(SInt, SByteArray))) if arr.length == 2 =>
        (arr(0).asInstanceOf[Int], builder.Colls.fromArray(arr(1).asInstanceOf[Array[Byte]]))
      case v =>
        sys.error(s"Invalid value $v of creationInfo register R3")
    }
  }

  override def tokens: Coll[(Coll[Byte], Long)] = {
    this.getReg[Coll[(Coll[Byte], Long)]](ErgoBox.R2.asIndex).get
  }

  override def executeFromRegister[T](regId: Byte)(implicit cT: RType[T]): T = ??? // TODO implement

  override def hashCode(): Int = id.toArray.hashCode()  // TODO optimize using just 4 bytes of id (since it is already hash)

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj != null && ( obj match {
    case obj: Box => util.Arrays.equals(id.toArray, obj.id.toArray)
  }))
}

object CostingBox {

  import Evaluation._

  def colBytes(b: Array[Byte])(implicit IR: Evaluation): Coll[Byte] = IR.sigmaDslBuilderValue.Colls.fromArray(b)

  def regs(ebox: ErgoBox, isCost: Boolean): Coll[AnyValue] = {
    val res = new Array[AnyValue](ErgoBox.maxRegisters)

    def checkNotYetDefined(id: Int, newValue: SValue) =
      require(res(id) == null, s"register $id is defined more then once: previous value ${res(id)}, new value $newValue")

    for ((k, v: EvaluatedValue[t]) <- ebox.additionalRegisters) {
      checkNotYetDefined(k.number, v)
      res(k.number) = toAnyValue(v.value)(stypeToRType(v.tpe))
    }

    for (r <- ErgoBox.mandatoryRegisters) {
      val regId = r.number
      val v = ebox.get(r).get.asInstanceOf[EvaluatedValue[SType]]
      checkNotYetDefined(regId, v)
      res(regId) = toAnyValue(v.value)(stypeToRType(v.tpe))
    }
    Colls.fromArray(res)
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
  * */
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
////    IR.verifyCostFunc(costF).getOrThrow
////    IR.verifyIsProven(calcF).getOrThrow
//
//    // check cost
////    val costingCtx = context.toSigmaContext(IR, isCost = true)
////    val costFun = IR.compile[SInt.type](IR.getDataEnv, costF)
////    val IntConstant(estimatedCost) = costFun(costingCtx)
////    if (estimatedCost > maxCost) {
////      throw new Error(s"Estimated execution cost $estimatedCost exceeds the limit $maxCost in $tree")
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
                       votes: Coll[Byte]
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
                    votes: Coll[Byte]
                  ) extends Header {
}

object CHeader {
  val VotesSize: Int = SigmaConstants.VotesArraySize.value
  val NonceSize: Int = SigmaConstants.AutolykosPowSolutionNonceArraySize.value
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

  def DeserializeRegister: Int = costOf("DeserializeRegister", SFunc(IndexedSeq(SBox, SByte), SOption(SOption.tT)))

  def SelectField: Int = costOf("SelectField", SFunc(IndexedSeq(), SUnit))

  def CollectionConst: Int = costOf("Const", SFunc(IndexedSeq(), SCollection(STypeVar("IV"))))

  def AccessKiloByteOfData: Int = costOf("AccessKiloByteOfData", SFunc(IndexedSeq(), SUnit))

  def PubKeySize: Long = CryptoConstants.EncodedGroupElementLength
}

class CostingSigmaDslBuilder extends TestSigmaDslBuilder { dsl =>
  implicit val validationSettings = ValidationRules.currentSettings

  override val Costing: CostedBuilder = new CCostedBuilder {

    import RType._

    override def defaultValue[T](valueType: RType[T]): T = (valueType match {
      case BooleanType => false
      case ByteType => 0.toByte
      case ShortType => 0.toShort
      case IntType => 0
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

  override def GroupElement(p: ECPoint): GroupElement = p match {
    case ept: EcPointType => CGroupElement(ept)
    case m => sys.error(s"Point of type ${m.getClass} is not supported")
  }

  def SigmaProp(sigmaTree: SigmaBoolean): SigmaProp = new CSigmaProp(sigmaTree)

  /** Extract `sigmastate.Values.SigmaBoolean` from DSL's `SigmaProp` type. */
  def toSigmaBoolean(p: SigmaProp): SigmaBoolean = p.asInstanceOf[CSigmaProp].sigmaTree

  /** Extract `sigmastate.AvlTreeData` from DSL's `AvlTree` type. */
  def toAvlTreeData(p: AvlTree): AvlTreeData = p.asInstanceOf[CAvlTree].treeData

  override def avlTree(operationFlags: Byte, digest: Coll[Byte], keyLength: Int, valueLengthOpt: Option[Int]): CAvlTree = {
    val treeData = AvlTreeData(ADDigest @@ digest.toArray, AvlTreeFlags(operationFlags), keyLength, valueLengthOpt)
    CAvlTree(treeData)
  }

  def avlTree(treeData: AvlTreeData): AvlTree = {
    CAvlTree(treeData)
  }

  def Box(ebox: ErgoBox): Box = CostingBox(false, ebox)
  def toErgoBox(b: Box): ErgoBox = b.asInstanceOf[CostingBox].ebox

  /** @hotspot don't beautify this code */
  private def toSigmaTrees(props: Array[SigmaProp]): Array[SigmaBoolean] = {
    val len = props.length
    val res = new Array[SigmaBoolean](len)
    cfor(0)(_ < len, _ + 1) { i =>
      res(i) = props(i) match {
        case csp: CSigmaProp => csp.sigmaTree
        case m: MockSigma => TrivialProp(m.isValid) //needed for tests, e.g. "atLeast" test
      }
    }
    res
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
    val r = SigmaSerializer.startReader(encoded.toArray)
    val p = GroupElementSerializer.parse(r)
    this.GroupElement(p)
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
  extends Context {
  @inline def builder: SigmaDslBuilder = CostingSigmaDslBuilder

  @inline def HEIGHT: Int = height

  @inline def SELF: Box = selfBox

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
      val optV =
        if (id < 0 || id >= vars.length) None
        else {
          val value = vars(id)
          if (value != null) {
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
      if (value != null) {
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
