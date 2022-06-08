package sigmastate

import Values._
import org.ergoplatform.Self
import org.typelevel.paiges.Doc
import sigmastate.serialization.OpCodes
import sigmastate.utxo.{ExtractRegisterAs, OptionGet, OptionIsDefined, SelectField}

/**
 * TODO: Docs - arguments/values naming $i
 * Brainstorm ideas: create configuration for printer (explicit types, curly/normal brackets)
 */
object PrettyPrintErgoTree {

  def prettyPrint(t: SValue, indentation: Int = 2, width: Int = 80): String = createDoc(t)(indentation).render(width)

  private def createDoc(t: SValue)(implicit i: Int): Doc = t match {
    case FalseLeaf => Doc.text("false")
    case TrueLeaf => Doc.text("true")
    case Self => Doc.text("SELF")

    case BinOr(l, r) => createDoc(l) + Doc.text(" || ") + createDoc(r)

    case ArithOp(left, right, OpCodes.PlusCode) => createDoc(left) + Doc.text(" + ") + createDoc(right)
    case ArithOp(left, right, OpCodes.MinusCode) => createDoc(left) + Doc.text(" - ") + createDoc(right)
    case ArithOp(left, right, OpCodes.MultiplyCode) => createDoc(left) + Doc.text(" * ") + createDoc(right)
    case ArithOp(left, right, OpCodes.DivisionCode) => createDoc(left) + Doc.text(" / ") + createDoc(right)
    case ArithOp(left, right, OpCodes.ModuloCode) => createDoc(left) + Doc.text(" % ") + createDoc(right)

    case ConstantNode(value, tpe) => Doc.text(s"$value.to$tpe")

    case OptionGet(x) => createDoc(x) + Doc.text(".get")
    case OptionIsDefined(x) => createDoc(x) + Doc.text(".isDefined")
    case SelectField(input, idx) => createDoc(input) + Doc.text(s"._$idx")

    // TODO: Check what is returned inside elemType when maybeTpe is None?
    case ExtractRegisterAs(input, registerId, maybeTpe) =>
      createDoc(input) + Doc.text(s".$registerId[${maybeTpe.elemType}]")

    case FuncValue(args, body) =>
      printArgs(args) + Doc.text(" => {") + Doc.line +
      createDoc(body).indent(i) + Doc.line +
      Doc.char('}')

    case ValDef(id, _, rhs) => Doc.text(s"val $$$id = ") + createDoc(rhs)
    case ValUse(id, tpe) => Doc.text(s"$$$id")

    case Tuple(items) =>
      Doc.char('(') + createDoc(items(0)) + Doc.char(',') + Doc.space + createDoc(items(1)) + Doc.char(')')

    case BlockValue(items, result) =>
      val prettyItems = items.map(item => createDoc(item))
      Doc.intercalate(Doc.line, prettyItems)+ Doc.line +
        createDoc(result)

    case If(condition, trueBranch, falseBranch) =>
      Doc.text("if (") + createDoc(condition) + Doc.text(") {") + Doc.line +
        createDoc(trueBranch).indent(i) + Doc.line +
        Doc.text("} else {") + Doc.line +
        createDoc(falseBranch).indent(i) + Doc.line +
        Doc.char('}')
  }

  /**
   * Create argument representation enclosed in brackets with types, e.g. `(foo: String, bar: Int)`
   */
  private def printArgs(args: Seq[(Int, SType)]): Doc = {
    val argsDoc = args.map { case (i, tpe) => Doc.text(s"$$$i: $tpe")}
    val body = Doc.intercalate(Doc.char(',') + Doc.line, argsDoc)
    body.tightBracketBy(Doc.char('('), Doc.char(')'))
  }

}