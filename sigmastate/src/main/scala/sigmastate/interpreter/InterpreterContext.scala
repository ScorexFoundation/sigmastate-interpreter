package sigmastate.interpreter

import org.ergoplatform.validation.SigmaValidationSettings
import sigmastate.SType
import sigmastate.Values.EvaluatedValue
import sigmastate.interpreter.ContextExtension.VarBinding
import sigmastate.serialization.SigmaSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import special.sigma
import special.sigma.AnyValue

/**
  * User-defined variables to be put into context.
  * Each variable is identified by `id: Byte` and can be accessed from a script
  * using `getVar[T](id)` operation.
  * The value of the variable is represented by [[sigmastate.Values.Constant]] instance,
  * which contains both data value and [[SType]] descriptor. The descriptor is checked
  * against the type `T` expected in the script operation. If the types don't match,
  * exception is thrown and the box spending (protected by the script) fails.
  *
  * @param values internal container of the key-value pairs
  */
case class ContextExtension(values: Map[Byte, EvaluatedValue[_ <: SType]]) {
  def add(bindings: VarBinding*): ContextExtension =
    ContextExtension(values ++ bindings)
}

object ContextExtension {
  val empty = ContextExtension(Map())
  type VarBinding = (Byte, EvaluatedValue[_ <: SType])

  object serializer extends SigmaSerializer[ContextExtension, ContextExtension] {

    override def serialize(obj: ContextExtension, w: SigmaByteWriter): Unit = {
      val size = obj.values.size
      if (size > Byte.MaxValue)
        error(s"Number of ContextExtension values $size exceeds ${Byte.MaxValue}.")
      w.putUByte(size)
      obj.values.foreach { case (id, v) => w.put(id).putValue(v) }
    }

    override def parse(r: SigmaByteReader): ContextExtension = {
      val extSize = r.getByte()
      if (extSize < 0)
        error(s"Negative amount of context extension values: $extSize")
      val ext = (0 until extSize)
        .map(_ => (r.getByte(), r.getValue().asInstanceOf[EvaluatedValue[_ <: SType]]))
        .toMap[Byte, EvaluatedValue[_ <: SType]]
      ContextExtension(ext)
    }

  }

}


/** Base class of the context passed to verifier and prover.
  * @see [[sigmastate.interpreter.Interpreter]]
  */
trait InterpreterContext {
  /** Prover-defined key-value pairs, that may be used inside a script. */
  val extension: ContextExtension

  /** Validation parameters passed to Interpreter.verify to detect soft-fork conditions. */
  val validationSettings: SigmaValidationSettings

  /** Hard limit on accumulated execution cost. Exceeding it leads to CostLimitException
    * to be thrown.
    */
  val costLimit: Long

  /** Initial value of execution cost already accumulated before `Interpreter.verify`(or
    * `prove`) is called.
    */
  val initCost: Long

  /** Maximum version of ErgoTree currently activated on the network. The activation is
    * performed via miners voting.
    * The maximum version supported by the interpreter is defined by
    * `Interpreter.MaxSupportedScriptVersion`. As a result, the execution of the
    * `Interpreter.verify` method depends on the relation between
    * max supported and activated version. (see docs/aot-jit-switch.md).
    */
  def activatedScriptVersion: Byte

  /** Creates a new instance with costLimit updated with given value. */
  def withCostLimit(newCostLimit: Long): InterpreterContext

  /** Creates a new instance with initCost updated with given value. */
  def withInitCost(newCost: Long): InterpreterContext

  /** Creates a new instance with extension updated with given value. */
  def withExtension(newExtension: ContextExtension): InterpreterContext

  /** Creates a new instance with given bindings added to extension. */
  def withBindings(bindings: VarBinding*): InterpreterContext = {
    val ext = extension.add(bindings: _*)
    withExtension(ext)
  }

  /** Creates a new instance with given validation settings. */
  def withValidationSettings(newVs: SigmaValidationSettings): InterpreterContext

  /** Creates `special.sigma.Context` instance based on this context. The created instance
    * contains all data represented using types form [[special.sigma]] package.
    * These types are used internally by ErgoTree interpreter.
    * Thus, this method performs transformation from Ergo to internal Sigma representation
    * of all context data.
    *
    * @param isCost     == true if the resulting context will be used in AOT cost estimation
    *                   otherwise it should be false
    * @param extensions additional context variables which will be merged with those in the
    *                   `extension` of this instance, overriding existing bindings in case
    *                   variable ids overlap.
    *
    * @see sigmastate.eval.Evaluation
    */
  def toSigmaContext(isCost: Boolean, extensions: Map[Byte, AnyValue] = Map()): sigma.Context
}

