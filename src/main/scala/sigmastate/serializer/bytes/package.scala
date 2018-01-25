package sigmastate.serializer

import scorex.core.serialization.Serializer
import sigmastate.{SInt, SType, Value}

import scala.reflect.runtime.universe._
import sigmastate.serializer.bytes.base._

package object bytes {
  def seqBytesSerializer[BS <: BytesSerializable](implicit bs: Serializer[BS]) = new SeqBytesSerializer[BS]

  implicit val heightSerializer: HeightSerializer = new HeightSerializer
  implicit val intConstantSerializer: IntConstantSerializer = new IntConstantSerializer
  implicit val booleanConstantSerializer: BooleanConstantSerializer = new BooleanConstantSerializer
//  implicit def concreteCollectionSerializer[T <: SType : TypeTag](implicit d: Serializer[Value[T]]): ConcreteCollectionSerializer[T] = new ConcreteCollectionSerializer[T]
  implicit val concreteCollectionSerializer: ConcreteCollectionSerializer = new ConcreteCollectionSerializer
}
