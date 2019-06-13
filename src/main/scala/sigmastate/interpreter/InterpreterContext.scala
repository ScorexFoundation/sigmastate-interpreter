package sigmastate.interpreter

import org.ergoplatform.validation.SigmaValidationSettings
import sigmastate.SType
import sigmastate.Values.EvaluatedValue
import sigmastate.eval.Evaluation
import sigmastate.serialization.SigmaSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import special.sigma
import special.sigma.AnyValue

/**
  * User-defined variables to be put into context
  *
  * @param values - key-value pairs
  */
case class ContextExtension(values: Map[Byte, EvaluatedValue[_ <: SType]]) {
  def add(bindings: (Byte, EvaluatedValue[_ <: SType])*): ContextExtension =
    ContextExtension(values ++ bindings)
}

object ContextExtension {
  val empty = ContextExtension(Map())

  object serializer extends SigmaSerializer[ContextExtension, ContextExtension] {

    override def serialize(obj: ContextExtension, w: SigmaByteWriter): Unit = {
      w.putUByte(obj.values.size)
      obj.values.foreach { case (id, v) => w.put(id).putValue(v) }
    }

    override def parse(r: SigmaByteReader): ContextExtension = {
      val extSize = r.getByte()
      val ext = (0 until extSize)
        .map(_ => (r.getByte(), r.getValue().asInstanceOf[EvaluatedValue[_ <: SType]]))
        .toMap[Byte, EvaluatedValue[_ <: SType]]
      ContextExtension(ext)
    }
  }

}


trait InterpreterContext {
  val extension: ContextExtension
  val validationSettings: SigmaValidationSettings

  def withExtension(newExtension: ContextExtension): InterpreterContext

  def withBindings(bindings: (Byte, EvaluatedValue[_ <: SType])*): InterpreterContext = {
    val ext = extension.add(bindings: _*)
    withExtension(ext)
  }

  def toSigmaContext(IR: Evaluation, isCost: Boolean, extensions: Map[Byte, AnyValue] = Map()): sigma.Context
}
