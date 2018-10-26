package sigmastate.interpreter

import sigmastate.SType
import sigmastate.Values.EvaluatedValue
import sigmastate.serialization.Serializer
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utils.Extensions._

/**
  * Variables to be put into context
  */
case class ContextExtension(values: Map[Byte, EvaluatedValue[_ <: SType]]) {
  def add(bindings: (Byte, EvaluatedValue[_ <: SType])*): ContextExtension =
    ContextExtension(values ++ bindings)
  //context values should not use context to determine their cost
  //todo: getOrElse(0L) branch triggers for local variables in Where/ForAll/Exists/Fold etc.
  //todo: Should the cost be 0 in this case?
  def cost(id: Byte): Long = values.get(id).map(_.cost(null)).getOrElse(0L)
}

object ContextExtension {
  val empty = ContextExtension(Map())

  object serializer extends Serializer[ContextExtension, ContextExtension] {

    override def serializeBody(obj: ContextExtension, w: ByteWriter): Unit = {
      w.putUByte(obj.values.size)
      obj.values.foreach{ case (id, v) => w.put(id).putValue(v) }
    }

    override def parseBody(r: ByteReader): ContextExtension = {
      val extSize = r.getByte()
      val ext = (0 until extSize)
        .map(_ => (r.getByte(), r.getValue().asInstanceOf[EvaluatedValue[_ <: SType]]))
        .toMap[Byte, EvaluatedValue[_ <: SType]]
      ContextExtension(ext)
    }
  }
}


trait Context{
  val extension: ContextExtension

  def withExtension(newExtension: ContextExtension): Context

  def withBindings(bindings: (Byte, EvaluatedValue[_ <: SType])*): Context = {
    val ext = extension.add(bindings:_*)
    withExtension(ext)
  }
}
