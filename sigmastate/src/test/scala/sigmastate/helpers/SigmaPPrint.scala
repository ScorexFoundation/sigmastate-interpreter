package sigmastate.helpers

import scala.collection.mutable
import pprint.{PPrinter, Tree}
import sigmastate.{SType, STypeCompanion}

/** Pretty-printer customized to print [[sigmastate.Values.Value]] instances
  * into a valid Scala code (can be cut-and-pasted).*/
object SigmaPPrint extends PPrinter {
  override val additionalHandlers: PartialFunction[Any, Tree] = {
    case t: STypeCompanion if t.isInstanceOf[SType] => Tree.Literal(s"S${t.typeName}")
    case v: Byte => Tree.Literal(s"$v.toByte")
    case wa: mutable.WrappedArray[_] => Tree.Apply("Array", wa.iterator.map(treeify))
  }
}

