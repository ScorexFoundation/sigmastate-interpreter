package sigmastate.helpers

import scala.collection.mutable
import pprint.{Tree, PPrinter}
import sigmastate.Values.ConstantNode
import sigmastate.utxo.SelectField
import sigmastate.{STypeCompanion, SPredefType, ArithOp, SType}

import scala.collection.mutable.ArrayBuffer

/** Pretty-printer customized to print [[sigmastate.Values.Value]] instances
  * into a valid Scala code (can be cut-and-pasted).*/
object SigmaPPrint extends PPrinter {

  def treeifyMany(xs: Seq[Any]): Iterator[Tree] = {
    xs.iterator.map(treeify)
  }

  def tpeName(tpe: SType): String = {
    val name = tpe.toTermString
    if (name == "Boolean") "Bool" else name
  }

  override val additionalHandlers: PartialFunction[Any, Tree] = {
    case t: STypeCompanion if t.isInstanceOf[SType] => Tree.Literal(s"S${t.typeName}")
    case v: Byte => Tree.Literal(s"$v.toByte")
    case wa: mutable.WrappedArray[_] =>
      Tree.Apply("Array", treeifyMany(wa))
    case buf: ArrayBuffer[_] =>
      Tree.Apply("Seq", treeifyMany(buf))
    case sf: SelectField =>
      val resTpe = sf.input.tpe.items(sf.fieldIndex - 1)
      val name = tpeName(resTpe)
      val resTpeName = name + "Value"
      Tree.Apply(s"SelectField.typed[$resTpeName]", treeifyMany(Array(sf.input, sf.fieldIndex)))
    case c: ConstantNode[_] if c.tpe.isInstanceOf[SPredefType] =>
      Tree.Apply(tpeName(c.tpe) + "Constant", treeifyMany(Seq(c.value)))
    case ArithOp(l, r, code) =>
      val args = treeifyMany(Seq(l, r)).toSeq :+ Tree.Apply("OpCode @@ ", treeifyMany(Seq(code)))
      Tree.Apply("ArithOp", args.iterator)

  }
}

