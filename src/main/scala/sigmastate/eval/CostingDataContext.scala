package sigmastate.eval

import java.math.BigInteger

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.ErgoBox
import scorex.crypto.authds.avltree.batch.{Lookup, Operation}
import scorex.crypto.authds.{ADKey, SerializedAdProof}
import sigmastate.SCollection.SByteArray
import sigmastate._
import sigmastate.Values.{Constant, EvaluatedValue, SValue, AvlTreeConstant, ConstantNode, SomeValue, NoneValue}
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.{CryptoConstants, Interpreter}
import sigmastate.serialization.{Serializer, OperationSerializer}
import special.collection.{CCostedBuilder, Col, Types}
import special.sigma._

import scala.reflect.ClassTag
import scala.util.{Success, Failure}
import scalan.meta.RType

case class CostingAvlTree(IR: Evaluation, treeData: AvlTreeData) extends AvlTree {
  override val builder = new CostingSigmaDslBuilder(IR)
  def startingDigest: Col[Byte] = builder.Cols.fromArray(treeData.startingDigest)

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

  override def getReg[T](i: Int)(implicit cT: RType[T]): Option[T] =
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
        val tpe = IR.elemToSType(cT.asInstanceOf[IR.Elem[_]])
        val default = builder.Costing.defaultValue(cT).asInstanceOf[SType#WrappedType]
        Some(Constant[SType](default, tpe).asInstanceOf[T])
      }
    } else
      super.getReg(i)

  override def creationInfo: (Int, Col[Byte]) = {
    import Types._
    this.R3[(Int, Col[Byte])].get.asInstanceOf[Any] match {
      case ConstantNode(arr: Array[Any], STuple(IndexedSeq(SLong, SByteArray))) if arr.length == 2 =>
        (arr(0).asInstanceOf[Int], builder.Cols.fromArray(arr(1).asInstanceOf[Array[Byte]]))
      case v =>
        sys.error(s"Invalid value $v of creationInfo register R3")
    }

  }
}

object CostingBox {

  def colBytes(b: Array[Byte])(implicit IR: Evaluation): Col[Byte] = IR.sigmaDslBuilderValue.Cols.fromArray(b)

  def regs(ebox: ErgoBox)(implicit IR: Evaluation): Col[AnyValue] = {
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
    IR.sigmaDslBuilderValue.Cols.fromArray(res)
  }

}

class CostingSigmaDslBuilder(val IR: Evaluation) extends TestSigmaDslBuilder { dsl =>
  override val Costing = new CCostedBuilder {
    import RType._
    override def defaultValue[T](valueType: RType[T]): T = (valueType match {
      case ByteType | IR.ByteElement  => 0.toByte
      case ShortType | IR.ShortElement=> 0.toShort
      case IntType | IR.IntElement  => 0
      case LongType | IR.LongElement => 0L
      case StringType | IR.StringElement => ""
      case p: PairRType[a, b] => (defaultValue(p.tA), defaultValue(p.tB))
      case col: Types.ColRType[a] => dsl.Cols.fromArray(Array[a]()(col.tA.classTag))
      case p: IR.PairElem[a, b] => (defaultValue(p.eFst), defaultValue(p.eSnd))
      case col: IR.Col.ColElem[a,_] => dsl.Cols.fromArray(Array[a]()(col.eItem.classTag))
      case avl: IR.AvlTree.AvlTreeElem[_] => CostingAvlTree(IR, AvlTreeData.dummy)
      case _ => sys.error(s"Cannot create defaultValue($valueType)")
    }).asInstanceOf[T]
  }

  override def treeLookup(tree: AvlTree, key: Col[Byte], proof: Col[Byte]) = {
    val keyBytes = key.arr
    val proofBytes = proof.arr
    val treeData = tree.asInstanceOf[CostingAvlTree].treeData
    val bv = AvlTreeConstant(treeData).createVerifier(SerializedAdProof @@ proofBytes)
    bv.performOneOperation(Lookup(ADKey @@ keyBytes)) match {
      case Failure(_) => Interpreter.error(s"Tree proof is incorrect $treeData")
      case Success(r) => r match {
        case Some(v) => Some(Cols.fromArray(v))
        case _ => None
      }
    }
  }

  override def treeModifications(tree: AvlTree, operations: Col[Byte], proof: Col[Byte]) = {
    val operationsBytes = operations.arr
    val proofBytes = proof.arr
    val treeData = tree.asInstanceOf[CostingAvlTree].treeData
    val bv = AvlTreeConstant(treeData).createVerifier(SerializedAdProof @@ proofBytes)
    val opSerializer = new OperationSerializer(bv.keyLength, bv.valueLengthOpt)
    val ops: Seq[Operation] = opSerializer.parseSeq(Serializer.startReader(operationsBytes, 0))
    ops.foreach(o => bv.performOneOperation(o))
    bv.digest match {
      case Some(v) => Some(Cols.fromArray(v))
      case _ => None
    }
  }

  override def exponentiate(base: ECPoint, exponent: BigInteger) = {
    CryptoConstants.dlogGroup.exponentiate(base.asInstanceOf[EcPointType], exponent)
  }

  override def atLeast(bound: Int, children: Col[SigmaProp]): SigmaProp = {
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

  override def getVar[T](id: Byte)(implicit cT: RType[T]) =
    if (isCost) {
      implicit val tag: ClassTag[T] = cT.classTag
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
        val tpe = IR.elemToSType(cT.asInstanceOf[IR.Elem[_]])
        val default = builder.Costing.defaultValue(cT).asInstanceOf[SType#WrappedType]
        Some(Constant[SType](default, tpe).asInstanceOf[T])
      }
    } else
      super.getVar(id)(cT)
}
