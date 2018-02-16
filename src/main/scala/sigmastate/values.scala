package sigmastate

import java.math.BigInteger

import com.google.common.primitives.Longs
import edu.biu.scapi.primitives.dlog.GroupElement
import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.authds.SerializedAdProof
import scorex.crypto.authds.avltree.batch.BatchAVLVerifier
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}
import sigmastate.utxo.{BoxWithMetadata, SigmaStateBox}
import sigmastate.utxo.CostTable.Cost

import scala.util.Try


/**
  *
  * @tparam V
  */
trait SigmaSerializer[V <: Value[_ <: SType]] extends Serializer[V] {
  import SigmaSerializer._

  val opCode: Byte

  def parseBody: DeserializingFn

  def serializeBody: SerializingFn[V]

  override def toBytes(obj: V): Array[Byte] = serialize(obj)

  override def parseBytes(bytes: Array[Byte]): Try[V] = Try {
    deserialize(bytes).asInstanceOf[V]
  }
}

object SigmaSerializer extends App {

  type Position = Int
  type Consumed = Int
  type DeserializingFn = (Array[Byte], Position) => (Value[_ <: SType], Consumed)
  type SerializingFn[V <: Value[_ <: SType]] = (V) => (Array[Byte])

  val GeCode = 21: Byte
  val GeDeserializer: DeserializingFn = {
    case (bytes, pos) =>
      val (firstArg, consumed) = deserialize(bytes, pos)
      val (secondArg, consumed2) = deserialize(bytes, pos + consumed)
      GE(firstArg.asInstanceOf[Value[SInt.type]], secondArg.asInstanceOf[Value[SInt.type]]) -> (consumed + consumed2)
  }
  val GeSerializer: SerializingFn[GE] = { cc => ???}

  val IntConstantCode = 11: Byte
  val IntConstantDeserializer: DeserializingFn = {
    case (bytes, pos) =>
      IntConstant(Longs.fromByteArray(bytes.slice(pos, pos + 8))) -> 8
  }
  val IntConstantSerializer: SerializingFn[IntConstant] = (c => Longs.toByteArray(c.value))


  val TrueCode = 12: Byte
  val TrueDeserializer: DeserializingFn = {
    case (bytes, pos) => TrueLeaf -> 0
  }
  val TrueSerializer: SerializingFn[TrueLeaf.type] = { cc => ???}

  val FalseCode = 13: Byte
  val FalseDeserializer: DeserializingFn = {
    case (bytes, pos) => FalseLeaf -> 0
  }
  val FalseSerializer: SerializingFn[FalseLeaf.type] = { cc => ???}

  val ConcreteCollectionCode = 25: Byte
  val ConcreteCollectionDeserializer: DeserializingFn = {
    case (bytes, pos) => ???
  }
  val ConcreteCollectionSerializer: SerializingFn[ConcreteCollection[_]] = { cc => ???}

  val table: Map[Value.PropositionCode, (DeserializingFn, SerializingFn[_])] = Map(
    GeCode -> (GeDeserializer, GeSerializer),
    ConcreteCollectionCode -> (ConcreteCollectionDeserializer, ConcreteCollectionSerializer),
    IntConstantCode -> (IntConstantDeserializer, IntConstantSerializer),
    TrueCode -> (TrueDeserializer, TrueSerializer),
    FalseCode -> (FalseDeserializer, FalseSerializer)
  )

  def deserialize(bytes: Array[Byte], pos: Int): (Value[_ <: SType], Consumed) = {
    val c = bytes(pos)
    val (v, consumed) = table(c)._1(bytes, pos + 1)
    v -> (consumed + 1)
  }

  def deserialize(bytes: Array[Byte]): Value[_ <: SType] = deserialize(bytes, 0)._1

  def serialize(v: Value[_]) = ???

  println(deserialize(Array[Byte](21, 11, 0, 0, 0, 0, 0, 0, 0, 2,
    11, 0, 0, 0, 0, 0, 0, 0, 3)))


  println(deserialize(Array[Byte](21, 12, 13)))

  import scala.reflect.runtime.universe.typeTag

  val s: Value[SInt.type] = IntConstant(4)
  println(typeTag[Value[SInt.type]])
}


trait Value[S <: SType] extends Product with Proposition {self: SType =>

  type M = Value[S]

  def cost: Int

  def evaluated: Boolean

  override def serializer: SigmaSerializer[M] = ???

  //todo: remove after serialization, replace with just .bytes
  lazy val propBytes = this.toString.getBytes

  //todo: remove this alias?
  def code: Value.PropositionCode = serializer.opCode
}

object Value {
  type PropositionCode = Byte
}

trait EvaluatedValue[S <: SType] extends Value[S] {
  val value: S#WrappedType
  override lazy val evaluated = true
}

trait NotReadyValue[S <: SType] extends Value[S] {
  override lazy val evaluated = false
}

trait TaggedVariable[S <: SType] extends NotReadyValue[S] {
  val id: Byte
}


//todo: make PreservingNonNegativeIntLeaf for registers which value should be preserved?
sealed trait IntLeaf extends Value[SInt.type]

case class IntConstant(value: Long) extends EvaluatedValue[SInt.type] {
  override val cost = 1
}

trait NotReadyValueInt extends NotReadyValue[SInt.type]

case object UnknownInt extends NotReadyValueInt {
  override val cost = 1
}

case class TaggedInt(override val id: Byte) extends TaggedVariable[SInt.type] with NotReadyValueInt {
  override val cost = 1
}


sealed trait BigIntLeaf extends Value[SBigInt.type]

case class BigIntConstant(value: BigInteger) extends EvaluatedValue[SBigInt.type] {
  override val cost = 1
}

trait NotReadyValueBigInt extends NotReadyValue[SBigInt.type] {
  override lazy val cost: Int = 1
}

case class TaggedBigInt(override val id: Byte) extends TaggedVariable[SBigInt.type] with NotReadyValueBigInt


case class ByteArrayConstant(value: Array[Byte]) extends EvaluatedValue[SByteArray.type] {

  override def cost: Int = ((value.length / 1024) + 1) * Cost.ByteArrayPerKilobyte

  override def equals(obj: scala.Any): Boolean = obj match {
    case ob: ByteArrayConstant => value sameElements ob.value
    case _ => false
  }
}

object EmptyByteArray extends ByteArrayConstant(Array.emptyByteArray)

trait NotReadyValueByteArray extends NotReadyValue[SByteArray.type] {
  override lazy val cost: Int = Cost.ByteArrayDeclaration
}

case object UnknownByteArray extends NotReadyValueByteArray


case class TaggedByteArray(override val id: Byte) extends TaggedVariable[SByteArray.type] with NotReadyValueByteArray


case class AvlTreeConstant(value: AvlTreeData) extends EvaluatedValue[SAvlTree.type] {
  override val cost = 50

  def createVerifier(proof: SerializedAdProof) =
    new BatchAVLVerifier[Digest32, Blake2b256Unsafe](
      value.startingDigest,
      proof,
      value.keyLength,
      value.valueLengthOpt,
      value.maxNumOperations,
      value.maxDeletes)
}


trait NotReadyValueAvlTree extends NotReadyValue[SAvlTree.type] {
  override val cost = 50
}

case class TaggedAvlTree(override val id: Byte) extends TaggedVariable[SAvlTree.type] with NotReadyValueAvlTree


case class GroupElementConstant(value: GroupElement) extends EvaluatedValue[SGroupElement.type] {
  override val cost = 10
}


case object GroupGenerator extends EvaluatedValue[SGroupElement.type] {
  override val cost = 10

  override val value: GroupElement = new BcDlogECFp().getGenerator
}


trait NotReadyValueGroupElement extends NotReadyValue[SGroupElement.type] {
  override val cost = 10
}

case class TaggedGroupElement(override val id: Byte)
  extends TaggedVariable[SGroupElement.type] with NotReadyValueGroupElement


sealed abstract class BooleanConstant(val value: Boolean) extends EvaluatedValue[SBoolean.type]

object BooleanConstant {
  def fromBoolean(v: Boolean): BooleanConstant = if (v) TrueLeaf else FalseLeaf
}

case object TrueLeaf extends BooleanConstant(true) {
  override def cost: Int = Cost.ConstantNode
}

case object FalseLeaf extends BooleanConstant(false) {
  override def cost: Int = Cost.ConstantNode
}

trait NotReadyValueBoolean extends NotReadyValue[SBoolean.type]

case class TaggedBoolean(override val id: Byte) extends TaggedVariable[SBoolean.type] with NotReadyValueBoolean {
  override def cost = 1
}

/**
  * For sigma statements
  */
trait SigmaBoolean extends NotReadyValue[SBoolean.type] {
  override lazy val evaluated = true
}


case class BoxConstant(value: SigmaStateBox) extends EvaluatedValue[SBox.type] {
  override def cost: Int = 10
}

trait NotReadyValueBox extends NotReadyValue[SBox.type] {
  override def cost: Int = 10
}

case class TaggedBox(override val id: Byte) extends TaggedVariable[SBox.type] with NotReadyValueBox


case class BoxWithMetadataConstant(value: BoxWithMetadata) extends EvaluatedValue[SBoxWithMetadata.type] {
  override def cost: Int = 10
}

trait NotReadyValueBoxWithMetadata extends NotReadyValue[SBoxWithMetadata.type] {
  override def cost: Int = 10
}

case class TaggedBoxWithMetadata(override val id: Byte)
  extends TaggedVariable[SBoxWithMetadata.type] with NotReadyValueBoxWithMetadata


case class ConcreteCollection[V <: SType](value: IndexedSeq[Value[V]]) extends EvaluatedValue[SCollection[V]] {
  val cost: Int = value.size
}

trait LazyCollection[V <: SType] extends NotReadyValue[SCollection[V]]