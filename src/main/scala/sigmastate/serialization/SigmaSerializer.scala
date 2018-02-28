package sigmastate.serialization

trait SigmaSerializer[TFamily, T <: TFamily] extends Serializer[T] {
  val companion: SigmaSerializerCompanion[TFamily]
  import companion._

  def parseBody(bytes: Array[Byte], pos: Position): (TFamily, Consumed)
  def serializeBody(obj: T): Array[Byte]
}

trait SigmaSerializerCompanion[TFamily] {
  type Position = Int
  type Consumed = Int
  type Tag
  val table: Map[Tag, SigmaSerializer[TFamily, _]]
  def deserialize(bytes: Array[Byte], pos: Position): (TFamily, Consumed)
  def serialize(v: TFamily): Array[Byte]
}

