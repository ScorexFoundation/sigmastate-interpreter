package sigmastate.eval

import java.math.BigInteger

import org.bouncycastle.math.ec.ECPoint
import org.bouncycastle.math.ec.custom.sec.SecP256K1Point
import org.ergoplatform.ErgoBox
import scorex.crypto.authds.avltree.batch.{Lookup, Operation}
import scorex.crypto.authds.{ADKey, SerializedAdProof}
import sigmastate.SCollection.SByteArray
import sigmastate._
import sigmastate.Values.{AvlTreeConstant, Constant, ConstantNode, EvaluatedValue, NoneValue, SValue, SomeValue}
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.{CryptoConstants, Interpreter}
import sigmastate.serialization.{OperationSerializer, Serializer}
import special.collection.{CCostedBuilder, Coll, CollType}
import special.sigma._

import scala.reflect.ClassTag
import scala.util.{Failure, Success}
import scalan.RType

case class CostingAvlTree(IR: Evaluation, treeData: AvlTreeData) extends AvlTree {
  override val builder = new CostingSigmaDslBuilder(IR)
  def startingDigest: Coll[Byte] = builder.Colls.fromArray(treeData.digest)

  def keyLength: Int = treeData.keyLength

  def valueLengthOpt: Option[Int] = treeData.valueLengthOpt

  def maxNumOperations: Option[Int] = treeData.maxNumOperations

  def maxDeletes: Option[Int] = treeData.maxDeletes

  def cost: Int = 1

  def dataSize: Long = SAvlTree.dataSize(treeData.asInstanceOf[SType#WrappedType])
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
    regs(ebox)(IR)
  )
{
  override val builder = new CostingSigmaDslBuilder(IR)

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
    this.R3[(Int, Coll[Byte])].get.asInstanceOf[Any] match {
      case ConstantNode(arr: Array[Any], STuple(IndexedSeq(SInt, SByteArray))) if arr.length == 2 =>
        (arr(0).asInstanceOf[Int], builder.Colls.fromArray(arr(1).asInstanceOf[Array[Byte]]))
      case v =>
        sys.error(s"Invalid value $v of creationInfo register R3")
    }

  }
}

object CostingBox {

  def colBytes(b: Array[Byte])(implicit IR: Evaluation): Coll[Byte] = IR.sigmaDslBuilderValue.Colls.fromArray(b)

  def regs(ebox: ErgoBox)(implicit IR: Evaluation): Coll[AnyValue] = {
    val res = new Array[AnyValue](ErgoBox.maxRegisters)

    def checkNotYetDefined(id: Int, newValue: SValue) =
      require(res(id) == null, s"register $id is defined more then once: previous value ${res(id)}, new value $newValue")

    for ((k, v) <- ebox.additionalRegisters) {
      checkNotYetDefined(k.number, v)
      res(k.number) = new TestValue(v)
    }

    for (r <- ErgoBox.mandatoryRegisters) {
      val regId = r.number
      val v = ebox.get(r).get
      checkNotYetDefined(regId, v)
      res(regId) = new TestValue(v)
    }
    IR.sigmaDslBuilderValue.Colls.fromArray(res)
  }

}

class CostingSigmaDslBuilder(val IR: Evaluation) extends TestSigmaDslBuilder { dsl =>
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
      case AvlTreeRType => CostingAvlTree(IR, AvlTreeData.dummy)
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

  override def exponentiate(base: SecP256K1Point, exponent: BigInteger) = {
    CryptoConstants.dlogGroup.exponentiate(base.asInstanceOf[EcPointType], exponent)
  }

  override def atLeast(bound: Int, children: Coll[SigmaProp]): SigmaProp = {
    Interpreter.error("Should not be called. Method calls of atLeast should be handled in Evaluation.compile.evaluate rule")
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
  override val builder = new CostingSigmaDslBuilder(IR)

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
