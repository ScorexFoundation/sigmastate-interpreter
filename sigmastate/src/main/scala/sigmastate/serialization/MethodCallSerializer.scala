package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.lang.SigmaTyper.STypeSubst
import sigmastate.lang.Terms.MethodCall
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.ComplexityTable
import spire.syntax.all.cfor

case class MethodCallSerializer(cons: (Value[SType], SMethod, IndexedSeq[Value[SType]], STypeSubst) => Value[SType])
  extends ValueSerializer[MethodCall] {
  override def opDesc: ValueCompanion = MethodCall

  override def serialize(mc: MethodCall, w: SigmaByteWriter): Unit = {
    w.put(mc.method.objType.typeId, ArgInfo("typeCode", "type of the method (see Table~\\ref{table:predeftypes})"))
    w.put(mc.method.methodId, ArgInfo("methodCode", "a code of the method"))
    w.putValue(mc.obj, ArgInfo("obj", "receiver object of this method call"))
    assert(mc.args.nonEmpty)
    w.putValues(mc.args, ArgInfo("args", "arguments of the method call"))
  }

  override def getComplexity: Int = 0  // because we add it explicitly in parse below

  /** The SMethod instances in STypeCompanions may have type STypeIdent in methods types,
    * but a valid ErgoTree should have SMethod instances specialized for specific types
    * of `obj` and `args` using `specializeFor`.
    * This means, if we save typeId, methodId, and we save all the arguments,
    * we can restore the specialized SMethod instance.
    * This work by induction, if we assume all arguments are monomorphic,
    * then we can make MethodCall monomorphic. Thus, all ErgoTree is monomorphic by construction.
    * This is limitation of MethodCall, because we cannot use it to represent for example
    * def Box.getReg[T](id: Int): Option[T], which require serialization of expected type `T`
    * However it can be implemented using separate node type (new type code) and can be added via soft-fork.
    * @hotspot don't beautify this code
    */
  override def parse(r: SigmaByteReader): Value[SType] = {
    val typeId = r.getByte()
    val methodId = r.getByte()
    val obj = r.getValue()
    val args = r.getValues()
    val method = SMethod.fromIds(typeId, methodId)
    val complexity = ComplexityTable.MethodCallComplexity.getOrElse((typeId, methodId), ComplexityTable.MinimalComplexity)
    r.addComplexity(complexity)
    val nArgs = args.length
    val types = new Array[SType](nArgs)
    cfor(0)(_ < nArgs, _ + 1) { i =>
      types(i) = args(i).tpe
    }
    val specMethod = method.specializeFor(obj.tpe, types)
    cons(obj, specMethod, args, Map.empty)
  }
}
