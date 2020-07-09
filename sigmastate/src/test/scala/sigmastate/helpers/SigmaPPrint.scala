package sigmastate.helpers

import org.ergoplatform.ErgoBox.RegisterId

import scala.collection.mutable
import pprint.{Tree, PPrinter}
import sigmastate.SCollection._
import sigmastate.Values.{ValueCompanion, ConstantNode}
import sigmastate.lang.SigmaTyper
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
    case SBooleanArray => Tree.Literal("SBooleanArray")
  }

  override val additionalHandlers: PartialFunction[Any, Tree] = typeHandlers.orElse {
    case sigmastate.SGlobal =>
      Tree.Literal(s"SGlobal")
    case sigmastate.SCollection =>
      Tree.Literal(s"SCollection")
    case t: STypeCompanion if t.isInstanceOf[SType] =>
      Tree.Literal(s"S${t.typeName}")
    case c: ValueCompanion =>
      Tree.Literal(c.typeName)
    case v: Byte =>
      Tree.Literal(s"$v.toByte")
    case wa: mutable.WrappedArray[_] =>
      Tree.Apply("Array", treeifyMany(wa))
    case buf: ArrayBuffer[_] =>
      Tree.Apply("Seq", treeifyMany(buf))
    case r: RegisterId =>
      Tree.Literal(s"ErgoBox.R${r.number}")
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
      val objType = apply(method.objType).plainText
      val methodTemplate = method.objType.getMethodByName(method.name)
      val methodT = SigmaTyper.unifyTypeLists(methodTemplate.stype.tDom, obj.tpe +: args.map(_.tpe)) match {
        case Some(subst) if subst.nonEmpty =>
          val getMethod = s"""$objType.getMethodByName("${method.name}").withConcreteTypes"""
          Tree.Apply(getMethod, treeifyMany(Seq(subst)))
        case _ =>
          val getMethod = s"$objType.getMethodByName"
          Tree.Apply(getMethod, Seq(treeify(method.name)).iterator)
      }

      val objT = treeify(obj)
      val argsT = treeify(args)
      val substT = treeify(typeSubst)
      val resTpe = mc.tpe
      val resTpeName = valueType(resTpe)
      Tree.Apply(s"MethodCall.typed[$resTpeName]", Seq(objT, methodT, argsT, substT).iterator)
  }
}

