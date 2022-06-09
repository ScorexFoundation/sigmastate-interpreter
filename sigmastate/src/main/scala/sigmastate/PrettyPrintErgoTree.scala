package sigmastate

import Values._
import org.ergoplatform.{Global, Outputs, Self}
import org.typelevel.paiges.Doc
import sigmastate.serialization.OpCodes
import sigmastate.utxo.{Exists, ExtractAmount, ExtractRegisterAs, OptionGet, OptionIsDefined, SelectField}
import sigmastate.lang.Terms.MethodCall

/**
 * TODO: Docs - arguments/values naming $i
 * Brainstorm ideas: create configuration for printer (explicit types, curly/normal brackets)
 */
object PrettyPrintErgoTree {

  def prettyPrint(t: SValue, indentation: Int = 2, width: Int = 80): String = createDoc(t)(indentation).render(width)

  private def createDoc(t: SValue)(implicit i: Int): Doc = t match {
    // Values
    case FalseLeaf => Doc.text("false")
    case TrueLeaf => Doc.text("true")
    case Tuple(items) => nTupleDoc(items.map(createDoc))
    case ValDef(id, _, rhs) => Doc.text(s"val $$$id = ") + createDoc(rhs)
    case ValUse(id, tpe) => Doc.text(s"$$$id")
    case ConstantNode(value, tpe) => Doc.text(s"$value.to") + STypeDoc(tpe)
    case FuncValue(args, body) =>
      Doc.char('{') + Doc.space + argsDoc(args) + Doc.text(" =>") + Doc.line +
        createDoc(body).indent(i) + Doc.line +
        Doc.char('}')
    case BlockValue(items, result) =>
      val prettyItems = items.map(item => createDoc(item))
      Doc.intercalate(Doc.line, prettyItems)+ Doc.line +
        createDoc(result)

    // ErgoLike
    case Self => Doc.text("SELF")
    case Global => Doc.text("Global")
    case Outputs => Doc.text("OUTPUTS")

    // Transformers
    case Exists(input, condition) => createDoc(input) + Doc.text(".exists(") + createDoc(condition) + Doc.char(')')
    case OptionGet(x) => createDoc(x) + Doc.text(".get")
    case OptionIsDefined(x) => createDoc(x) + Doc.text(".isDefined")
    case SelectField(input, idx) => createDoc(input) + Doc.text(s"._$idx")
    // TODO: Check what is returned inside elemType when maybeTpe is None?
    case ExtractRegisterAs(input, registerId, maybeTpe) =>
      createDoc(input) + Doc.text(s".$registerId[") + STypeDoc(maybeTpe.elemType) + Doc.char(']')
    case ExtractAmount(input) => createDoc(input) + Doc.text(".value")

    // Terms
    case MethodCall(obj, method, args, map) =>
      val argS = if (args.nonEmpty) Doc.intercalate(Doc.comma + Doc.space, args.map(createDoc)) else Doc.empty
      createDoc(obj) + Doc.char('.') + Doc.text(method.name) + argS

    // Trees
    case If(condition, trueBranch, falseBranch) =>
      Doc.text("if (") + createDoc(condition) + Doc.text(") {") + Doc.line +
        createDoc(trueBranch).indent(i) + Doc.line +
        Doc.text("} else {") + Doc.line +
        createDoc(falseBranch).indent(i) + Doc.line +
        Doc.char('}')
    case BinOr(l, r) => createDoc(l) + Doc.text(" || ") + createDoc(r)
    case ArithOp(left, right, OpCodes.PlusCode) => createDoc(left) + Doc.text(" + ") + createDoc(right)
    case ArithOp(left, right, OpCodes.MinusCode) => createDoc(left) + Doc.text(" - ") + createDoc(right)
    case ArithOp(left, right, OpCodes.MultiplyCode) => createDoc(left) + Doc.text(" * ") + createDoc(right)
    case ArithOp(left, right, OpCodes.DivisionCode) => createDoc(left) + Doc.text(" / ") + createDoc(right)
    case ArithOp(left, right, OpCodes.ModuloCode) => createDoc(left) + Doc.text(" % ") + createDoc(right)
    case GT(left, right) => createDoc(left) + Doc.text(" > ") + createDoc(right)
    // TODO: not covered by test
    case LT(left, right) => createDoc(left) + Doc.text(" < ") + createDoc(right)
  }

  /* Create argument representation enclosed in brackets with types, e.g. `($5: String, $1: Int)` */
  private def argsDoc(args: Seq[(Int, SType)]): Doc = {
    val argsWithTypes = args.map { case (i, tpe) => Doc.text(s"$$$i:") + Doc.space + STypeDoc(tpe) }
    nTupleDoc(argsWithTypes)
  }

  private def STypeDoc(tpe: SType): Doc = tpe match {
    case SShort => Doc.text("Short")
    case SInt => Doc.text("Int")
    case SLong => Doc.text("Long")
    case SBox => Doc.text("Box")
    case SCollectionType(elemType) => Doc.text("Coll[") + STypeDoc(elemType) + Doc.char(']')
    case STuple(items) => nTupleDoc(items.map(STypeDoc))
  }

  private def nTupleDoc(items: Seq[Doc]): Doc =
    Doc
      .intercalate(Doc.comma + Doc.space, items)
      .tightBracketBy(Doc.char('('), Doc.char(')'))

}