package sigmastate.interpreter

import io.circe._
import io.circe.syntax._
import org.ergoplatform.JsonCodecs
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

object ContextExtension extends JsonCodecs {
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

  implicit val jsonEncoder: Encoder[ContextExtension] = { extension =>
    extension.values.map { case (key, value) =>
      key -> evaluatedValueEncoder(value)
    }.asJson
  }
}


trait InterpreterContext {
  val extension: ContextExtension
  val validationSettings: SigmaValidationSettings
  val costLimit: Long
  val initCost: Long

  /** Creates a new instance with costLimit updated with given value. */
  def withCostLimit(newCostLimit: Long): InterpreterContext

  /** Creates a new instance with initCost updated with given value. */
  def withInitCost(newCost: Long): InterpreterContext

  /** Creates a new instance with extension updated with given value. */
  def withExtension(newExtension: ContextExtension): InterpreterContext

  /** Creates a new instance with given bindings added to extension. */
  def withBindings(bindings: (Byte, EvaluatedValue[_ <: SType])*): InterpreterContext = {
    val ext = extension.add(bindings: _*)
    withExtension(ext)
  }

  /** Creates a new instance with given validation settings. */
  def withValidationSettings(newVs: SigmaValidationSettings): InterpreterContext

  /** Creates `special.sigma.Context` instance based on this context. */
  def toSigmaContext(IR: Evaluation, isCost: Boolean, extensions: Map[Byte, AnyValue] = Map()): sigma.Context
}
