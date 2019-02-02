package sigmastate.interpreter

import sigmastate.SType
import sigmastate.Values.EvaluatedValue
import sigmastate.eval.Evaluation
import sigmastate.serialization.Serializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigma.util.Extensions._
import special.sigma

/**
  * Variables to be put into context
  */
case class ContextExtension(values: Map[Byte, EvaluatedValue[_ <: SType]]) {
  def add(bindings: (Byte, EvaluatedValue[_ <: SType])*): ContextExtension =
    ContextExtension(values ++ bindings)
}

object ContextExtension {
  val empty = ContextExtension(Map())

  object serializer extends Serializer[ContextExtension, ContextExtension] {

    override def serializeBody(obj: ContextExtension, w: SigmaByteWriter): Unit = {
      w.putUByte(obj.values.size)
      obj.values.foreach{ case (id, v) => w.put(id).putValue(v) }
    }

    override def parseBody(r: SigmaByteReader): ContextExtension = {
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

  def toSigmaContext(IR: Evaluation, isCost: Boolean): sigma.Context
}
