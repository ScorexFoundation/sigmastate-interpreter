package sigmastate.helpers

import java.math.BigInteger

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.RegisterId
import org.ergoplatform.settings.ErgoAlgos

import scala.collection.mutable
import pprint.{Tree, PPrinter}
import scalan.RType
import sigmastate.SCollection._
import sigmastate.Values.{ValueCompanion, ConstantNode, ErgoTree}
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.lang.SigmaTyper
import sigmastate.lang.Terms.MethodCall
import sigmastate.serialization.GroupElementSerializer
import sigmastate.utxo.SelectField
import sigmastate._
import special.collection.Coll
import special.sigma.GroupElement

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/** Pretty-printer customized to print [[sigmastate.Values.Value]] instances
  * into a valid Scala code (can be cut-and-pasted).*/
object SigmaPPrint extends PPrinter {

  def treeifySeq(xs: Seq[Any]): Iterator[Tree] = {
    xs.iterator.map(_ match {
      case t: Tree => t
      case x => treeify(x)
    })
  }

  def treeifyMany(head: Any, tail: Any*): Iterator[Tree] = {
    treeifySeq(head +: tail)
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
    case SByteArray =>
      Tree.Literal("SByteArray")
    case SByteArray2 =>
      Tree.Literal("SByteArray2")
    case SBooleanArray =>
      Tree.Literal("SBooleanArray")
    case SPair(l, r) =>
      Tree.Apply("SPair", treeifySeq(Array(l, r)))
  }

  val exceptionHandlers: PartialFunction[Any, Tree] = {
    case ex: RuntimeException =>
      Tree.Apply(s"new ${ex.getClass.getSimpleName}", treeifySeq(Seq(ex.getMessage)))
  }

  def treeifyByteArray(bytes: Array[Byte]): Tree = {
    val hexString = ErgoAlgos.encode(bytes)
    Tree.Apply("ErgoAlgos.decodeUnsafe", treeifyMany(hexString))
  }

  val dataHandlers: PartialFunction[Any, Tree] = {
    case v: Byte =>
      Tree.Literal(s"$v.toByte")

    case v: Short =>
      Tree.Literal(s"$v.toShort")

    case v: BigInteger =>
      Tree.Apply("new BigInteger", treeifyMany(v.toString(16), 16))

    case wa: mutable.WrappedArray[Byte @unchecked] if wa.elemTag == ClassTag.Byte =>
      treeifyByteArray(wa.array)

    case wa: mutable.WrappedArray[_] =>
      Tree.Apply("Array", treeifySeq(wa))

    case arr: Array[Byte @unchecked] if arr.elemTag == ClassTag.Byte =>
      treeifyByteArray(arr)

    case arr: Array[_] =>
      Tree.Apply("Array", treeifySeq(arr))

    case buf: ArrayBuffer[_] =>
      Tree.Apply("Seq", treeifySeq(buf))

    case ecp: EcPointType =>
      val hexString = ErgoAlgos.encode(GroupElementSerializer.toBytes(ecp))
      Tree.Apply("Helpers.decodeECPoint", treeifyMany(hexString))

    case ge: GroupElement =>
      val hexString = ErgoAlgos.encode(ge.getEncoded)
      Tree.Apply("Helpers.decodeGroupElement", treeifyMany(hexString))

    case coll: Coll[Byte @unchecked] if coll.tItem == RType.ByteType =>
      val hexString = ErgoAlgos.encode(coll)
      Tree.Apply("Helpers.decodeBytes", treeifyMany(hexString))

    case t: AvlTreeData =>
      Tree.Apply("AvlTreeData", treeifyMany(
        Tree.Apply("ADDigest @@ ", treeifyMany(t.digest)),
        t.treeFlags,
        t.keyLength,
        t.valueLengthOpt))

    case t: ErgoTree =>
      Tree.Apply("new ErgoTree", treeifyMany(
        t.header,
        Tree.Apply("Vector", t.constants.map(treeify).iterator),
        t.root
      ))

    case b: ErgoBox =>
      val tokens = Tree.Apply("Coll",
        b.additionalTokens.toArray.map { case (id, v) =>
          val idTree = Tree.Apply("Digest32 @@ ", treeifyMany(id))
          Tree.Apply("", treeifyMany(idTree, v))
        }.iterator)
      Tree.Apply("new ErgoBox", treeifyMany(
        b.value,
        b.ergoTree,
        tokens,
        b.additionalRegisters,
        Tree.Apply("ModifierId @@ ", treeifyMany(b.transactionId)),
        b.index,
        b.creationHeight
      ))
  }

  override val additionalHandlers: PartialFunction[Any, Tree] =
    typeHandlers
     .orElse(exceptionHandlers)
     .orElse(dataHandlers)
     .orElse {
    case sigmastate.SGlobal =>
      Tree.Literal(s"SGlobal")
    case sigmastate.SCollection =>
      Tree.Literal(s"SCollection")
    case sigmastate.SOption =>
      Tree.Literal(s"SOption")
    case t: STypeCompanion if t.isInstanceOf[SType] =>
      Tree.Literal(s"S${t.typeName}")
    case c: ValueCompanion =>
      Tree.Literal(c.typeName)
    case r: RegisterId =>
      Tree.Literal(s"ErgoBox.R${r.number}")
    case sf: SelectField =>
      val resTpe = sf.input.tpe.items(sf.fieldIndex - 1)
      val resTpeName = valueType(resTpe)
      Tree.Apply(s"SelectField.typed[$resTpeName]", treeifySeq(Array(sf.input, sf.fieldIndex)))

    case ConstantNode(v, SCollectionType(elemType)) if elemType.isInstanceOf[SPredefType] =>
      Tree.Apply(tpeName(elemType) + "ArrayConstant", treeifySeq(Seq(v)))

    case c: ConstantNode[_] if c.tpe.isInstanceOf[SPredefType] =>
      Tree.Apply(tpeName(c.tpe) + "Constant", treeifySeq(Seq(c.value)))

    case ArithOp(l, r, code) =>
      val args = treeifySeq(Seq(l, r)).toSeq :+ Tree.Apply("OpCode @@ ", treeifySeq(Seq(code)))
      Tree.Apply("ArithOp", args.iterator)
    case mc @ MethodCall(obj, method, args, typeSubst) =>
      val objType = apply(method.objType).plainText
      val methodTemplate = method.objType.getMethodByName(method.name)
      val methodT = SigmaTyper.unifyTypeLists(methodTemplate.stype.tDom, obj.tpe +: args.map(_.tpe)) match {
        case Some(subst) if subst.nonEmpty =>
          val getMethod = s"""$objType.getMethodByName("${method.name}").withConcreteTypes"""
          Tree.Apply(getMethod, treeifySeq(Seq(subst)))
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

