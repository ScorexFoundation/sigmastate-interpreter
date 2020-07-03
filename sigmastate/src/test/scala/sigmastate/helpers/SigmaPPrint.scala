package sigmastate.helpers

import scala.collection.mutable
import pprint.{Tree, PPrinter}
import sigmastate.SCollection._
import sigmastate.Values.ConstantNode
import sigmastate.lang.Terms.MethodCall
import sigmastate.utxo.SelectField
import sigmastate.{SByte, STypeCompanion, STuple, ArithOp, SOption, SType, SCollectionType, SPredefType, SCollection}

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

  def typeName(tpe: SType): String = tpe match {
    case _: SPredefType =>
      val name = tpe.getClass.getSimpleName.replace("$", "")
      s"$name.type" // SByte.type, SInt.type, etc
    case ct: SCollectionType[_] =>
      s"SCollection[${typeName(ct.elemType)}]"
    case ot: SOption[_] =>
      s"SOption[${typeName(ot.elemType)}]"
    case _: STuple =>
      "STuple"
    case _ =>
      sys.error(s"Cannot get typeName($tpe)")
  }

  def valueType(tpe: SType): String = {
    val tn = typeName(tpe)
    s"Value[$tn]"
  }

  val typeHandlers: PartialFunction[Any, Tree] = {
    case SByteArray => Tree.Literal("SByteArray")
    case SByteArray2 => Tree.Literal("SByteArray2")
  }

  override val additionalHandlers: PartialFunction[Any, Tree] = typeHandlers.orElse {
    case t: STypeCompanion if t.isInstanceOf[SType] => Tree.Literal(s"S${t.typeName}")
    case v: Byte => Tree.Literal(s"$v.toByte")
    case wa: mutable.WrappedArray[_] =>
      Tree.Apply("Array", treeifyMany(wa))
    case buf: ArrayBuffer[_] =>
      Tree.Apply("Seq", treeifyMany(buf))
    case sf: SelectField =>
      val resTpe = sf.input.tpe.items(sf.fieldIndex - 1)
      val resTpeName = valueType(resTpe)
      Tree.Apply(s"SelectField.typed[$resTpeName]", treeifyMany(Array(sf.input, sf.fieldIndex)))
    case c: ConstantNode[_] if c.tpe.isInstanceOf[SPredefType] =>
      Tree.Apply(tpeName(c.tpe) + "Constant", treeifyMany(Seq(c.value)))
    case ArithOp(l, r, code) =>
      val args = treeifyMany(Seq(l, r)).toSeq :+ Tree.Apply("OpCode @@ ", treeifyMany(Seq(code)))
      Tree.Apply("ArithOp", args.iterator)
    case mc @ MethodCall(obj, method, args, typeSubst) =>
      val objType = treeify(method.objType).asInstanceOf[Tree.Literal].body
      val getMethod = s"$objType.getMethodByName"
      val objT = treeify(obj)
      val methodT = Tree.Apply(getMethod, Seq(treeify(method.name)).iterator)
      val argsT = treeify(args)
      val substT = treeify(typeSubst)
      val resTpe = mc.tpe
      val resTpeName = valueType(resTpe)
      Tree.Apply(s"MethodCall.typed[$resTpeName]", Seq(objT, methodT, argsT, substT).iterator)
  }
}

