package sigmastate.interpreter

import sigmastate.SType
import sigmastate.Values.EvaluatedValue
import sigmastate.serialization.{Serializer, ValueSerializer}
import sigmastate.serialization.Serializer.{Consumed, Position}

import scala.collection.mutable
import scala.util.Try

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
    override def toBytes(ext: ContextExtension): Array[Byte] = {
      val extSize = ext.values.size.toByte
      extSize +: ext.values.foldLeft(Array[Byte]()){case (ba, (id, value)) =>
        ba ++ (id +: ValueSerializer.serialize(value))
      }
    }

    override def parseBytes(bytes: Array[Byte]): Try[ContextExtension] =
      Try(parseBody(bytes, 0)._1)

    override def parseBody(bytes: Array[Byte], pos: Position): (ContextExtension, Consumed) = {
      val extSize = bytes(pos)
      val ext = mutable.Map[Byte, EvaluatedValue[_ <: SType]]()

      var p = pos + 1
      (0 until extSize).foreach{_ =>
        val id = bytes(p)
        val (v, consumed)  = ValueSerializer.deserialize(bytes, p + 1)
        ext.put(id, v.asInstanceOf[EvaluatedValue[_ <: SType]])
        p = p + 1 + consumed
      }
      ContextExtension(ext.toMap) -> (p - pos)
    }
  }
}


trait Context[C <: Context[C]] {
  val extension: ContextExtension

  def withExtension(newExtension: ContextExtension): C

  def withBindings(bindings: (Byte, EvaluatedValue[_ <: SType])*): Context[_] = {
    val ext = extension.add(bindings:_*)
    withExtension(ext)
  }
}
