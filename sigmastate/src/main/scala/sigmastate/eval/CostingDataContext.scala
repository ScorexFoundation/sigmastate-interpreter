package sigmastate.eval

import com.google.common.primitives.{Ints, Longs}

import java.math.BigInteger
import java.util.Arrays
import org.ergoplatform.{ErgoBox, SigmaConstants}
import org.ergoplatform.validation.ValidationRules
import scalan.OverloadHack.Overloaded1
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADDigest, ADKey, ADValue, SerializedAdProof}
import sigmastate.SCollection.SByteArray
import sigmastate.{TrivialProp, _}
import sigmastate.Values.{ConstantNode, ErgoTree, EvaluatedValue, SValue, SigmaBoolean}
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.{CryptoConstants, Interpreter}
import special.collection._
import special.sigma._
import sigmastate.eval.Extensions._
import debox.cfor

import scala.util.{Failure, Success}
import scalan.util.Extensions.BigIntegerOps
import scalan.{Nullable, RType}
import scorex.crypto.hash.{Blake2b256, Digest32, Sha256}
import sigmastate.Values.ErgoTree.EmptyConstants
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.ProveDHTuple
import sigmastate.crypto.{CryptoFacade, Ecp}
import sigmastate.lang.TransformingSigmaBuilder
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.serialization.{GroupElementSerializer, SigmaSerializer}

import scala.reflect.ClassTag

/** Interface implmented by wrappers to provide access to the underlying wrapped value. */
trait WrapperOf[T] {
  /** The data value wrapped by this wrapper. */
  def wrappedValue: T
}

/** A default implementation of [[BigInt]] interface.
  * @see [[BigInt]] for detailed descriptions
  */
case class CBigInt(override val wrappedValue: BigInteger) extends BigInt with WrapperOf[BigInteger] {
  val dsl = CostingSigmaDslBuilder
  override def toByte : Byte  = wrappedValue.byteValueExact()
  override def toShort: Short = wrappedValue.shortValueExact()
  override def toInt  : Int   = wrappedValue.intValueExact()
  override def toLong : Long  = wrappedValue.longValueExact()

  override def toBytes: Coll[Byte] = dsl.Colls.fromArray(wrappedValue.toByteArray)

  override def toAbs: BigInt = dsl.BigInt(wrappedValue.abs())

  override def compareTo(that: BigInt): Int =
    wrappedValue.compareTo(that.asInstanceOf[CBigInt].wrappedValue)

  override def toBits: Coll[Boolean] = ???

  override def modQ: BigInt = ???

  override def plusModQ(other: BigInt): BigInt = ???

  override def minusModQ(other: BigInt): BigInt = ???

  override def multModQ(other: BigInt): BigInt = ???

  override def inverseModQ: BigInt = ???

  override def signum: Int = wrappedValue.signum()

  override def add(that: BigInt): BigInt = dsl.BigInt(wrappedValue.add(that.asInstanceOf[CBigInt].wrappedValue).to256BitValueExact)

  override def subtract(that: BigInt): BigInt = dsl.BigInt(wrappedValue.subtract(that.asInstanceOf[CBigInt].wrappedValue).to256BitValueExact)

  override def multiply(that: BigInt): BigInt = dsl.BigInt(wrappedValue.multiply(that.asInstanceOf[CBigInt].wrappedValue).to256BitValueExact)

  override def divide(that: BigInt): BigInt = dsl.BigInt(wrappedValue.divide(that.asInstanceOf[CBigInt].wrappedValue))

  override def mod(m: BigInt): BigInt = dsl.BigInt(wrappedValue.mod(m.asInstanceOf[CBigInt].wrappedValue))

  override def remainder(that: BigInt): BigInt = dsl.BigInt(wrappedValue.remainder(that.asInstanceOf[CBigInt].wrappedValue))

  override def min(that: BigInt): BigInt = dsl.BigInt(wrappedValue.min(that.asInstanceOf[CBigInt].wrappedValue))

  override def max(that: BigInt): BigInt = dsl.BigInt(wrappedValue.max(that.asInstanceOf[CBigInt].wrappedValue))

  override def negate(): BigInt = dsl.BigInt(wrappedValue.negate().to256BitValueExact)

  override def and(that: BigInt): BigInt = dsl.BigInt(wrappedValue.and(that.asInstanceOf[CBigInt].wrappedValue))

  override def or(that: BigInt): BigInt = dsl.BigInt(wrappedValue.or(that.asInstanceOf[CBigInt].wrappedValue))
}

/** A default implementation of [[GroupElement]] interface.
  * @see [[GroupElement]] for detailed descriptions
  */
case class CGroupElement(override val wrappedValue: Ecp) extends GroupElement with WrapperOf[Ecp] {
  val dsl = CostingSigmaDslBuilder
  
  override def toString: String = s"GroupElement(${Extensions.showECPoint(wrappedValue)})"

  override def getEncoded: Coll[Byte] =
    dsl.Colls.fromArray(GroupElementSerializer.toBytes(wrappedValue))

  override def isInfinity: Boolean = CryptoFacade.isInfinityPoint(wrappedValue)

  override def exp(k: BigInt): GroupElement =
    dsl.GroupElement(CryptoFacade.multiplyPoint(wrappedValue, k.asInstanceOf[CBigInt].wrappedValue))

  override def multiply(that: GroupElement): GroupElement =
    dsl.GroupElement(CryptoFacade.addPoint(wrappedValue, that.asInstanceOf[CGroupElement].wrappedValue))

  override def negate: GroupElement =
    dsl.GroupElement(CryptoFacade.negatePoint(wrappedValue))
}

/** A default implementation of [[SigmaProp]] interface.
  * @see [[SigmaProp]] for detailed descriptions
  */
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
    // TODO v6.0: add `pk.propBytes(version)`
    val root = sigmaTree.toSigmaProp
    val ergoTree = new ErgoTree(ErgoTree.DefaultHeader, EmptyConstants, Right(root), 0, null, None)
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

/** Implementation of the [[special.sigma.AvlTreeVerifier]] trait based on
  * [[scorex.crypto.authds.avltree.batch.BatchAVLVerifier]].
  *
  * @see BatchAVLVerifier, AvlTreeVerifier
  */
class CAvlTreeVerifier(startingDigest: ADDigest,
                       proof: SerializedAdProof,
                       override val keyLength: Int,
                       override val valueLengthOpt: Option[Int])
    extends BatchAVLVerifier[Digest32, Blake2b256.type](
      startingDigest, proof, keyLength, valueLengthOpt)
        with AvlTreeVerifier {
  override def treeHeight: Int = rootNodeHeight

  /** Override default logging which outputs stack trace to the console. */
  override protected def logError(t: Throwable): Unit = {}
}

/** A default implementation of [[AvlTree]] interface.
  * @see [[AvlTree]] for detailed descriptions
  */
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

  override def digest: Coll[Byte] = Colls.fromArray(treeData.digest)

  override def updateDigest(newDigest: Coll[Byte]): AvlTree = {
    val td = treeData.copy(digest = ADDigest @@ newDigest.toArray)
    this.copy(treeData = td)
  }

  override def createVerifier(proof: Coll[Byte]): AvlTreeVerifier = {
    val adProof = SerializedAdProof @@ proof.toArray
    val bv = new CAvlTreeVerifier(
      treeData.digest, adProof, treeData.keyLength, treeData.valueLengthOpt)
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

/** Default implementation of AnyValue interface. */
case class CAnyValue[A](value: A, tVal: RType[Any]) extends AnyValue {
  def tA: RType[A] = tVal.asInstanceOf[RType[A]]
  override def toString = s"TestValue($value)"
}

object CAnyValue {
  def apply[A](value: A, t: RType[A])(implicit o: Overloaded1): CAnyValue[A] =
    new CAnyValue(value, t.asInstanceOf[RType[Any]])
}

import sigmastate.eval.CostingBox._

/** A default implementation of [[Box]] interface.
  * @see [[Box]] for detailed descriptions
  */
case class CostingBox(val ebox: ErgoBox) extends Box with WrapperOf[ErgoBox] {
  val builder = CostingSigmaDslBuilder

  val value = ebox.value
  lazy val id: Coll[Byte] = Colls.fromArray(ebox.id)
  lazy val bytes: Coll[Byte] = Colls.fromArray(ebox.bytes)
  lazy val bytesWithoutRef: Coll[Byte] = Colls.fromArray(ebox.bytesWithNoRef)
  lazy val propositionBytes: Coll[Byte] = Colls.fromArray(ebox.propositionBytes)
  lazy val registers: Coll[AnyValue] = regs(ebox)

  override def wrappedValue: ErgoBox = ebox

  override def getReg[T](i: Int)(implicit tT: RType[T]): Option[T] = {
    if (i < 0 || i >= registers.length) return None
    val value = registers(i)
    if (value != null ) {
      // once the value is not null it should be of the right type
      value match {
        case value: CAnyValue[_] if value.value != null && value.tA == tT =>
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

  override def hashCode(): Int = Ints.fromByteArray(id.toArray)

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj != null && {
    obj match {
      case obj: Box => Arrays.equals(id.toArray, obj.id.toArray)
      case _ =>
        // this case was missing in v4.x, however has never been a problem
        // Thus, v5.0 interpreter will not fail (while v4.x would fail here)
        false
    }
  })
}

object CostingBox {

  import Evaluation._

  def regs(ebox: ErgoBox): Coll[AnyValue] = {
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

/** A default implementation of [[PreHeader]] interface.
  * @see [[PreHeader]] for detailed descriptions
  */
case class CPreHeader(
                       version: Byte,
                       parentId: Coll[Byte],
                       timestamp: Long,
                       nBits: Long,
                       height: Int,
                       minerPk: GroupElement,
                       votes: Coll[Byte]
                     ) extends PreHeader {}

/** A default implementation of [[Header]] interface.
  * @see [[Header]] for detailed descriptions
  */
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

/** A default implementation of [[SigmaDslBuilder]] interface.
  * @see [[SigmaDslBuilder]] for detailed descriptions
  */
class CostingSigmaDslBuilder extends SigmaDslBuilder { dsl =>
  implicit val validationSettings = ValidationRules.currentSettings

  // manual fix
  override val Colls: CollBuilder = new CollOverArrayBuilder

  override def BigInt(n: BigInteger): BigInt = CBigInt(n)

  override def toBigInteger(n: BigInt): BigInteger = n.asInstanceOf[CBigInt].wrappedValue

  def GroupElement(p: Ecp): GroupElement = p match {
    case ept: EcPointType => CGroupElement(ept)
    case m => sys.error(s"Point of type ${m.getClass} is not supported")
  }

  /** Wraps the given sigma proposition into SigmaDsl value of type SigmaProp. */
  def SigmaProp(sigmaTree: SigmaBoolean): SigmaProp = new CSigmaProp(sigmaTree)

  /** Extract `sigmastate.Values.SigmaBoolean` from DSL's `SigmaProp` type. */
  @inline def toSigmaBoolean(p: SigmaProp): SigmaBoolean = p.asInstanceOf[CSigmaProp].sigmaTree

  /** Extract `sigmastate.AvlTreeData` from DSL's `AvlTree` type. */
  def toAvlTreeData(p: AvlTree): AvlTreeData = p.asInstanceOf[CAvlTree].treeData

  /** Extract `sigmastate.crypto.Ecp` from DSL's `GroupElement` type. */
  def toECPoint(ge: GroupElement): Ecp = ge.asInstanceOf[CGroupElement].wrappedValue

  override def avlTree(operationFlags: Byte, digest: Coll[Byte], keyLength: Int, valueLengthOpt: Option[Int]): CAvlTree = {
    val treeData = AvlTreeData(ADDigest @@ digest.toArray, AvlTreeFlags(operationFlags), keyLength, valueLengthOpt)
    CAvlTree(treeData)
  }

  /** Wraps the given tree data into SigmaDsl value of type [[AvlTree]]. */
  def avlTree(treeData: AvlTreeData): AvlTree = {
    CAvlTree(treeData)
  }

  /** Wraps the given [[ErgoBox]] into SigmaDsl value of type [[Box]].
    * @param ebox  the value to be wrapped
    * @see [[sigmastate.SBox]], [[special.sigma.Box]]
    */
  def Box(ebox: ErgoBox): Box = CostingBox(ebox)

  /** Extracts [[ErgoBox]] from the given [[Box]] instance. This is inverse to the Box method. */
  def toErgoBox(b: Box): ErgoBox = b.asInstanceOf[CostingBox].ebox

  /** HOTSPOT: don't beautify this code */
  private def toSigmaTrees(props: Array[SigmaProp]): Array[SigmaBoolean] = {
    val len = props.length
    val res = new Array[SigmaBoolean](len)
    cfor(0)(_ < len, _ + 1) { i =>
      res(i) = toSigmaBoolean(props(i))
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

  override def allOf(conditions: Coll[Boolean]): Boolean =
    conditions.forall(c => c)

  override def anyOf(conditions: Coll[Boolean]): Boolean =
    conditions.exists(c => c)

  override def xorOf(conditions: Coll[Boolean]): Boolean = {
    if (VersionContext.current.isJitActivated) {
      val len = conditions.length
      if (len == 0) false
      else if (len == 1) conditions(0)
      else {
        var res = conditions(0)
        cfor(1)(_ < len, _ + 1) { i =>
          res ^= conditions(i)
        }
        res
      }
    } else {
      // This is buggy version used in v4.x interpreter (for ErgoTrees v0, v1)
      conditions.toArray.distinct.length == 2
    }
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

  override def xor(l: Coll[Byte], r: Coll[Byte]): Coll[Byte] =
    Colls.xor(l, r)

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

  override def byteArrayToBigInt(bytes: Coll[Byte]): BigInt = {
    val bi = new BigInteger(bytes.toArray).to256BitValueExact
    this.BigInt(bi)
  }

  override def longToByteArray(l: Long): Coll[Byte] =
    Colls.fromArray(Longs.toByteArray(l))

  override def byteArrayToLong(bytes: Coll[Byte]): Long =
    Longs.fromByteArray(bytes.toArray)

  override def proveDlog(ge: GroupElement): SigmaProp =
    CSigmaProp(ProveDlog(toECPoint(ge).asInstanceOf[EcPointType]))

  override def proveDHTuple(g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement): SigmaProp = {
    val dht = ProveDHTuple(toEcPointType(g), toEcPointType(h), toEcPointType(u), toEcPointType(v))
    CSigmaProp(dht)
  }

  private lazy val _generatorElement = this.GroupElement(CryptoConstants.dlogGroup.generator)
  override def groupGenerator: GroupElement = _generatorElement

  /**
    * @return the identity of the Dlog group used in ErgoTree
    */
  def groupIdentity: GroupElement = {
    this.GroupElement(CryptoConstants.dlogGroup.identity)
  }

  override def substConstants[T](scriptBytes: Coll[Byte],
                                 positions: Coll[Int],
                                 newValues: Coll[T]): Coll[Byte] = {
    val typedNewVals = newValues.toArray.map(v => TransformingSigmaBuilder.liftToConstant(v) match {
      case Nullable(v) => v
      case _ => sys.error(s"Cannot evaluate substConstants($scriptBytes, $positions, $newValues): cannot lift value $v")
    })

    val (res, _) = SubstConstants.eval(scriptBytes.toArray, positions.toArray, typedNewVals)(validationSettings)
    Colls.fromArray(res)
  }

  override def decodePoint(encoded: Coll[Byte]): GroupElement = {
    val r = SigmaSerializer.startReader(encoded.toArray)
    val p = GroupElementSerializer.parse(r)
    this.GroupElement(p)
  }

}

/** Default singleton instance of Global object, which implements global ErgoTree functions. */
object CostingSigmaDslBuilder extends CostingSigmaDslBuilder

/** A default implementation of [[Context]] interface.
  * @see [[Context]] for detailed descriptions
  */
case class CostingDataContext(
                               _dataInputs: Coll[Box],
                               override val headers: Coll[Header],
                               override val preHeader: PreHeader,
                               inputs: Coll[Box],
                               outputs: Coll[Box],
                               height: Int,
                               selfBox: Box,
                               private val selfIndex: Int,
                               lastBlockUtxoRootHash: AvlTree,
                               _minerPubKey: Coll[Byte],
                               vars: Coll[AnyValue],
                               override val activatedScriptVersion: Byte,
                               override val currentErgoTreeVersion: Byte
                               )
  extends Context {
  @inline override def builder: SigmaDslBuilder = CostingSigmaDslBuilder

  @inline override def HEIGHT: Int = height

  @inline override def SELF: Box = selfBox

  @inline override def dataInputs: Coll[Box] = _dataInputs

  @inline override def INPUTS = inputs

  @inline override def OUTPUTS = outputs

  @inline override def LastBlockUtxoRootHash = lastBlockUtxoRootHash

  @inline override def minerPubKey = _minerPubKey

  override def selfBoxIndex: Int = {
    if (VersionContext.current.isJitActivated) {
      // starting from v5.0 this is fixed
      selfIndex
    } else {
      // this used to be a bug in v4.x (https://github.com/ScorexFoundation/sigmastate-interpreter/issues/603)
      -1
    }
  }

  override def getVar[T](id: Byte)(implicit tT: RType[T]): Option[T] = {
    implicit val tag: ClassTag[T] = tT.classTag
    if (id < 0 || id >= vars.length) return None
    val value = vars(id)
    if (value != null) {
      // once the value is not null it should be of the right type
      value match {
        case value: CAnyValue[_] if value.value != null && value.tA == tT =>
          Some(value.value.asInstanceOf[T])
        case _ =>
          throw new InvalidType(s"Cannot getVar[${tT.name}]($id): invalid type of value $value at id=$id")
      }
    } else None
  }

  /** Return a new context instance with variables collection updated.
    * @param bindings  a new binding of the context variables with new values.
    * @return a new instance (if `bindings` non-empty) with the specified bindings.
    *         other existing bindings are copied to the new instance
    */
  def withUpdatedVars(bindings: (Int, AnyValue)*): CostingDataContext = {
    if (bindings.isEmpty) return this

    val ids = bindings.map(_._1).toArray
    val values = bindings.map(_._2).toArray
    val maxVarId = ids.max  // INV: ids is not empty
    val requiredNewLength = maxVarId + 1

    val newVars = if (vars.length < requiredNewLength) {
      // grow vars collection
      val currVars = vars.toArray
      val buf = new Array[AnyValue](requiredNewLength)
      Array.copy(currVars, 0, buf, 0, currVars.length)
      cfor(0)(_ < ids.length, _ + 1) { i =>
        buf(ids(i)) = values(i)
      }
      buf.toColl
    } else {
      vars.updateMany(ids.toColl, values.toColl)
    }

    this.copy(vars = newVars)
  }
}

