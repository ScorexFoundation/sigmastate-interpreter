package sigmastate.eval

import java.awt.MultipleGradientPaint.ColorSpaceType
import java.math.BigInteger

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.{ErgoLikeContext, ErgoBox}
import scorex.crypto.authds.avltree.batch.{Lookup, Operation}
import scorex.crypto.authds.{ADKey, SerializedAdProof}
import sigmastate.SCollection.SByteArray
import sigmastate._
import sigmastate.Values.{Constant, EvaluatedValue, SValue, AvlTreeConstant, ConstantNode, SigmaPropConstant, SomeValue, Value, ErgoTree, SigmaBoolean, GroupElementConstant, NoneValue}
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.{CryptoConstants, Interpreter}
import sigmastate.serialization.{ValueSerializer, ErgoTreeSerializer, Serializer, OperationSerializer}
import special.collection.{Coll, CCostedBuilder, CollType, Builder}
import special.sigma._

import scala.reflect.ClassTag
import scala.util.{Success, Failure}
import scalan.RType
import scorex.crypto.hash.{Sha256, Blake2b256}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.ProveDHTuple
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer

case class CostingSigmaProp(sigmaTree: SigmaBoolean) extends SigmaProp {
  override def isValid: Boolean = sigmaTree match {
    case TrivialProp(cond) => cond
    case _ => sys.error(s"Method CostingSigmaProp.isValid is not defined for $sigmaTree")
  }

  override def propBytes: Coll[Byte] = {
    val bytes = DefaultSerializer.serializeWithSegregation(SigmaPropConstant(sigmaTree))
    Builder.DefaultCollBuilder.fromArray(bytes)
  }

  override def &&(other: SigmaProp): SigmaProp = other match {
    case other: CostingSigmaProp =>
      CostingSigmaProp(CAND.normalized(Seq(sigmaTree, other.sigmaTree)))
  }

  override def &&(other: Boolean): SigmaProp =
    CostingSigmaProp(CAND.normalized(Seq(sigmaTree, TrivialProp(other))))

  override def ||(other: SigmaProp): SigmaProp = other match {
    case other: CostingSigmaProp =>
      CostingSigmaProp(COR.normalized(Seq(sigmaTree, other.sigmaTree)))
  }

  override def ||(other: Boolean): SigmaProp =
    CostingSigmaProp(COR.normalized(Seq(sigmaTree, TrivialProp(other))))
}

case class CostingAvlTree(treeData: AvlTreeData) extends AvlTree {
  val builder = new CostingSigmaDslBuilder()
  def startingDigest: Coll[Byte] = builder.Colls.fromArray(treeData.startingDigest)

  def keyLength: Int = treeData.keyLength

  def valueLengthOpt: Option[Int] = treeData.valueLengthOpt

  def maxNumOperations: Option[Int] = treeData.maxNumOperations

  def maxDeletes: Option[Int] = treeData.maxDeletes

  def cost: Int = 1

  def dataSize: Long = SAvlTree.dataSize(treeData.asInstanceOf[SType#WrappedType])

  override def digest: Coll[Byte] = builder.Colls.fromArray(treeData.startingDigest)
}

import CostingBox._

class CostingBox(val IR: Evaluation,
                 isCost: Boolean,
                 val ebox: ErgoBox)
  extends TestBox(
    colBytes(ebox.id)(IR),
    ebox.value,
    colBytes(ebox.bytes)(IR),
    colBytes(ebox.bytesWithNoRef)(IR),
    colBytes(ebox.propositionBytes)(IR),
    regs(ebox, isCost)(IR)
  )
{
  override val builder = new CostingSigmaDslBuilder()

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
      case ConstantNode(arr: Array[Any], STuple(IndexedSeq(SInt, SByteArray))) if arr.length == 2 =>
        (arr(0).asInstanceOf[Int], builder.Colls.fromArray(arr(1).asInstanceOf[Array[Byte]]))
      case v =>
        sys.error(s"Invalid value $v of creationInfo register R3")
    }

  }
}

object CostingBox {

  def colBytes(b: Array[Byte])(implicit IR: Evaluation): Coll[Byte] = IR.sigmaDslBuilderValue.Colls.fromArray(b)

  def regs(ebox: ErgoBox, isCost: Boolean)(implicit IR: Evaluation): Coll[AnyValue] = {
    val res = new Array[AnyValue](ErgoBox.maxRegisters)

    def checkNotYetDefined(id: Int, newValue: SValue) =
      require(res(id) == null, s"register $id is defined more then once: previous value ${res(id)}, new value $newValue")

    for ((k, v: SValue) <- ebox.additionalRegisters) {
      checkNotYetDefined(k.number, v)
      res(k.number) = new TestValue(ErgoLikeContext.toTestData(v, v.tpe, isCost))
    }

    for (r <- ErgoBox.mandatoryRegisters) {
      val regId = r.number
      val v = ebox.get(r).get
      checkNotYetDefined(regId, v)
      res(regId) = new TestValue(ErgoLikeContext.toTestData(v, v.tpe, isCost))
    }
    IR.sigmaDslBuilderValue.Colls.fromArray(res)
  }

}

class CostingSigmaDslBuilder extends TestSigmaDslBuilder { dsl =>
  override val Costing = new CCostedBuilder {
    import RType._
    override def defaultValue[T](valueType: RType[T]): T = (valueType match {
      case BooleanType  => false
      case ByteType => 0.toByte
      case ShortType => 0.toShort
      case IntType  => 0
      case LongType => 0L
      case StringType => ""
      case p: PairType[a, b] => (defaultValue(p.tFst), defaultValue(p.tSnd))
      case col: CollType[a] => dsl.Colls.emptyColl(col.tItem)
      case AvlTreeRType => CostingAvlTree(AvlTreeData.dummy)
      case _ => sys.error(s"Cannot create defaultValue($valueType)")
    }).asInstanceOf[T]
  }

  override def treeLookup(tree: AvlTree, key: Coll[Byte], proof: Coll[Byte]) = {
    val keyBytes = key.toArray
    val proofBytes = proof.toArray
    val treeData = tree.asInstanceOf[CostingAvlTree].treeData
    val bv = AvlTreeConstant(treeData).createVerifier(SerializedAdProof @@ proofBytes)
    bv.performOneOperation(Lookup(ADKey @@ keyBytes)) match {
      case Failure(_) => Interpreter.error(s"Tree proof is incorrect $treeData")
      case Success(r) => r match {
        case Some(v) => Some(Colls.fromArray(v))
        case _ => None
      }
    }
  }

  override def treeModifications(tree: AvlTree, operations: Coll[Byte], proof: Coll[Byte]) = {
    val operationsBytes = operations.toArray
    val proofBytes = proof.toArray
    val treeData = tree.asInstanceOf[CostingAvlTree].treeData
    val bv = AvlTreeConstant(treeData).createVerifier(SerializedAdProof @@ proofBytes)
    val opSerializer = new OperationSerializer(bv.keyLength, bv.valueLengthOpt)
    val ops: Seq[Operation] = opSerializer.parseSeq(Serializer.startReader(operationsBytes, 0))
    ops.foreach(o => bv.performOneOperation(o))
    bv.digest match {
      case Some(v) => Some(Colls.fromArray(v))
      case _ => None
    }
  }

  override def exponentiate(base: ECPoint, exponent: BigInt) = {
    CryptoConstants.dlogGroup.exponentiate(base.asInstanceOf[EcPointType], toBigInteger(exponent))
  }

  private def toSigmaTrees(props: Array[SigmaProp]): Array[SigmaBoolean] = {
    props.map { case csp: CostingSigmaProp => csp.sigmaTree }
  }

  private def toGroupElementConst(p: ECPoint): GroupElementConstant =
    GroupElementConstant(p.asInstanceOf[EcPointType])

  override def atLeast(bound: Int, props: Coll[SigmaProp]): SigmaProp = {
    val sigmaTrees = toSigmaTrees(props.toArray)
    val tree = AtLeast.reduce(bound, sigmaTrees)
    CostingSigmaProp(tree)
  }

  override def allZK(props: Coll[SigmaProp]): SigmaProp = {
    val sigmaTrees = toSigmaTrees(props.toArray)
    val tree = CAND.normalized(sigmaTrees)
    CostingSigmaProp(tree)
  }

  override def anyZK(props: Coll[SigmaProp]): SigmaProp = {
    val sigmaTrees = toSigmaTrees(props.toArray)
    val tree = COR.normalized(sigmaTrees)
    CostingSigmaProp(tree)
  }

  override def sigmaProp(b: Boolean): SigmaProp = {
    CostingSigmaProp(TrivialProp(b))
  }

  override def blake2b256(bytes: Coll[Byte]): Coll[Byte] = {
    val h = Blake2b256.hash(bytes.toArray)
    Colls.fromArray(h)
  }

  override def sha256(bytes: Coll[Byte]): Coll[Byte] = {
    val h = Sha256.hash(bytes.toArray)
    Colls.fromArray(h)
  }

  override def proveDlog(g: ECPoint): SigmaProp =
    CostingSigmaProp(ProveDlog(g.asInstanceOf[EcPointType]))

  override def proveDHTuple(g: ECPoint, h: ECPoint, u: ECPoint, v: ECPoint): SigmaProp = {
    val dht = ProveDHTuple(
      toGroupElementConst(g), toGroupElementConst(g),
      toGroupElementConst(u), toGroupElementConst(v))
    CostingSigmaProp(dht)
  }

  override def groupGenerator: ECPoint = {
    CryptoConstants.dlogGroup.generator
  }

  override def substConstants[T](scriptBytes: Coll[Byte],
      positions: Coll[Int],
      newValues: Coll[T])
      (implicit cT: RType[T]): Coll[Byte] = {
    val typedNewVals = newValues.toArray.map(_.asInstanceOf[Value[SType]])
    val res = SubstConstants.eval(scriptBytes.toArray, positions.toArray, typedNewVals)
    Colls.fromArray(res)
  }

  override def decodePoint(encoded: Coll[Byte]): ECPoint = {
    CryptoConstants.dlogGroup.curve.decodePoint(encoded.toArray)
  }
}

class CostingDataContext(
    val IR: Evaluation,
    inputs: Array[Box],
    outputs: Array[Box],
    height: Int,
    selfBox: Box,
    lastBlockUtxoRootHash: AvlTree,
    minerPubKey: Array[Byte],
    vars: Array[AnyValue],
    var isCost: Boolean)
    extends TestContext(inputs, outputs, height, selfBox, lastBlockUtxoRootHash, minerPubKey, vars)
{
  override val builder = new CostingSigmaDslBuilder()

  override def getVar[T](id: Byte)(implicit tT: RType[T]) =
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
    } else
      super.getVar(id)(tT)
}
