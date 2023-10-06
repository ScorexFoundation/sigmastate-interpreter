package sigmastate.interpreter

import sigma.ast.EvaluatedValue
import sigmastate.interpreter.ContextExtension.VarBinding
import sigma.serialization.{SigmaByteReader, SigmaByteWriter, SigmaSerializer}
import sigma.AnyValue
import sigma.ast.SType
import sigma.validation.SigmaValidationSettings

import scala.collection.mutable

/**
  * User-defined variables to be put into context.
  * Each variable is identified by `id: Byte` and can be accessed from a script
  * using `getVar[T](id)` operation.
  * The value of the variable is represented by [[sigma.ast.Constant]] instance,
  * which contains both data value and [[SType]] descriptor. The descriptor is checked
  * against the type `T` expected in the script operation. If the types don't match,
  * exception is thrown and the box spending (protected by the script) fails.
  *
  * @param values internal container of the key-value pairs
  */
case class ContextExtension(values: scala.collection.Map[Byte, EvaluatedValue[_ <: SType]]) {
  def add(bindings: VarBinding*): ContextExtension =
    ContextExtension(values ++ bindings)
}

object ContextExtension {
  /** Immutable instance of empty ContextExtension, which can be shared to avoid
    * allocations. */
  val empty = ContextExtension(Map())

  /** Type of context variable binding. */
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
      ContextExtension(mutable.LinkedHashMap(ext:_*))
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

  /** Creates a new instance with currErgoTreeVersion updated with the given value. */
  def withErgoTreeVersion(newVersion: Byte): InterpreterContext

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

  /** Creates `sigma.Context` instance based on this context. The created instance
    * contains all data represented using types form [[sigma]] package.
    * These types are used internally by ErgoTree interpreter.
    * Thus, this method performs transformation from Ergo to internal Sigma representation
    * of all context data.
    *
    * @param extensions additional context variables which will be merged with those in the
    *                   `extension` of this instance, overriding existing bindings in case
    *                   variable ids overlap.
    *
    * @see sigmastate.eval.Evaluation
    */
  def toSigmaContext(extensions: Map[Byte, AnyValue] = Map()): sigma.Context
}

