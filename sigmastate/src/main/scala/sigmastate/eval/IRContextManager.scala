package sigmastate.eval

import java.lang.ref.WeakReference

import scala.util.DynamicVariable

/** Provides a capability to execute actions with a IRContext instance. */
abstract class IRContextManager {
  /** Returns the [[IRContextFactory]] used by this manager. */
  def getIRContextFactory: IRContextFactory

  /** Executes the given function with the underlying context.
    * @param action function to be executed
    * @return the result returned by the action
    */
  def executeWithIRContext[T](action: IRContext => T): T
}

/** An implementation of [[sigmastate.eval.IRContextManager]] which uses the given `irFactory`
  * to create a fresh new IRContext instance for every invocation of
  * [[sigmastate.eval.IRContextManager#executeWithIRContext]].
  */
class ReallocatingIRContextManager(irFactory: IRContextFactory) extends IRContextManager {
  import ReallocatingIRContextManager._

  override def getIRContextFactory: IRContextFactory = irFactory

  override def executeWithIRContext[T](action: IRContext => T): T = {
    val newIR = irFactory.createIRContext
    if (newIR eq _IR.value)
      sys.error(s"Factory should return a fresh instance of IRContext on every call.")
    _IR.value = newIR

    if (_inProcess.value)
      sys.error(s"Nested executeWithContext is not supported")

    _inProcess.withValue(true) {
      action(newIR)
    }
  }
}

object ReallocatingIRContextManager {
  private val _IR = new DynamicVariable[IRContext](null)
  private val _inProcess = new DynamicVariable(false)
}

/** An implementation of [[sigmastate.eval.IRContextManager]] which uses the given `irFactory`
  * to create a new IRContext instance once and then reset it when the given capacity is exceeded.
  * If capacity == UndefinedCapacity, the reset happens before EVERY execution.
  * The reset is done by [[scalan.Base#resetContext]] before `action` is executed.
  *
  * The created context is stored under [[WeakReference]] placed in
  * [[InheritableThreadLocal]] by using [[DynamicVariable]].
  * Each instance of [[ResettingIRContextManager]] is using it's own [[DynamicVariable]].
  *
  * Thus, [[IRContext]]s created by different managers are isolated, and in addition
  * contexts created by the same manager, but on different threads are also isolated and
  * there is not way to access IRContexts created on other threads.
  * At the same time, an instance of ResettingIRContextManager can be created on any thread.
  *
  * Using [[WeakReference]] allows to reclaim memory allocated on now inactive threads and
  * by different manager instances. When any such [[WeakReference]] stored in
  * DynamicVariable is cleared, the given `irFactory` is used to recreate a fresh new
  * [[IRContext]] instance, which is saved back in the DynamicVariable.
  *
  * @param irFactory   factory to create new IRContext instances
  * @param capacity    if greater than 0 specifies a maximum number of the graph nodes in the context
  */
class ResettingIRContextManager(
    irFactory: IRContextFactory,
    capacity: Int = ResettingIRContextManager.UndefinedCapacity
  ) extends IRContextManager {
  import ResettingIRContextManager._

  override def getIRContextFactory: IRContextFactory = irFactory

  private val _IR = new DynamicVariable[WeakReference[IRContext]](null)
  private val _inProcess = new DynamicVariable(false)

  /** Obtains the valid [[IRContext]] from thread local [[WeakReference]] (cached value).
    * Uses the `irFactory` to create a fresh new IRContext instance if necessary
    * if cached value is not available.
    * @return an instance of IRContext stored in the thread local cache
    */
  private def obtainIR: IRContext = {
    val currRef = _IR.value
    if (currRef == null) {
      // first access on the current thread, create both new IR and the ref
      val ir = irFactory.createIRContext
      _IR.value = new WeakReference(ir)
      ir
    } else {
      var ir = currRef.get()  // try resolving via weak reference
      if (ir == null) {
        // the reference was cleared, recreate
        println("the reference was cleared")
        ir = irFactory.createIRContext
        _IR.value = new WeakReference(ir)
      }
      ir
    }
  }

  override def executeWithIRContext[T](action: IRContext => T): T = {
    val currIR = obtainIR

    // reset the context when the capacity is not defined or the node counter is too high
    if (capacity == UndefinedCapacity || currIR.defCount > capacity) {
      currIR.resetContext()
    }

    if (_inProcess.value)
      sys.error(s"Nested executeWithContext is not supported")

    _inProcess.withValue(true) {
      action(currIR)
    }
  }
}

object ResettingIRContextManager {
  val UndefinedCapacity = 0
  val DefaultCapacity = 100000
}
