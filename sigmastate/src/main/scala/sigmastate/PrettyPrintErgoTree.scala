package sigmastate

import Values._
import org.ergoplatform.Self
import sigmastate.serialization.OpCodes
import sigmastate.utxo.{ExtractRegisterAs, OptionGet, OptionIsDefined, SelectField}

/**
 * TODO: Docs - arguments/values naming $i
 * Brainstorm ideas: create configuration for printer (line length, leading spaces for new line, explicit types)
 */
object PrettyPrintErgoTree {

  def prettyPrint(t: SValue): String = t match {
    case FalseLeaf => "false"
    case TrueLeaf => "true"
    case Self => "SELF"

    case BinOr(l, r) => s"${prettyPrint(l)} || ${prettyPrint(r)}"

    case ArithOp(left, right, OpCodes.PlusCode) => s"${prettyPrint(left)} + ${prettyPrint(right)}"
    case ArithOp(left, right, OpCodes.MinusCode) => s"${prettyPrint(left)} - ${prettyPrint(right)}"
    case ArithOp(left, right, OpCodes.MultiplyCode) => s"${prettyPrint(left)} * ${prettyPrint(right)}"
    case ArithOp(left, right, OpCodes.DivisionCode) => s"${prettyPrint(left)} / ${prettyPrint(right)}"
    case ArithOp(left, right, OpCodes.ModuloCode) => s"${prettyPrint(left)} % ${prettyPrint(right)}"

    case ConstantNode(value, tpe) => s"$value.to$tpe"

    case OptionGet(x) => s"${prettyPrint(x)}.get"
    case OptionIsDefined(x) => s"${prettyPrint(x)}.isDefined"
    case SelectField(input, idx) => s"${prettyPrint(input)}._$idx"

    // TODO: Check what is returned inside elemType when maybeTpe is None?
    case ExtractRegisterAs(input, registerId, maybeTpe) => s"${prettyPrint(input)}.$registerId[${maybeTpe.elemType}]"

    case FuncValue(args, body) =>
      s"""${printArgs(args)} => {
         |${prettyPrint(body)}
         |}""".stripMargin

    case ValDef(id, _, rhs) => s"val $$$id = ${prettyPrint(rhs)}"
    case ValUse(id, tpe) => s"$$$id"

    case Tuple(items) => s"(${prettyPrint(items(0))}, ${prettyPrint(items(1))})"

    case BlockValue(items, result) =>
      val prettyItems = items.map(item => prettyPrint(item))
      val prettyResult = prettyPrint(result)
      prettyItems.mkString("", "\n", "\n") + prettyResult

    case If(condition, trueBranch, falseBranch) =>
      s"""if (${prettyPrint(condition)}) {
         |${prettyPrint(trueBranch)}
         |} else {
         |${prettyPrint(falseBranch)}
         |}""".stripMargin
  }

  /**
   * Create argument representation enclosed in brackets with types, e.g. `(foo: String, bar: Int)`
   */
  private def printArgs(args: Seq[(Int, SType)]): String = {
    args
      .map { case (i, tpe) => s"$$$i: $tpe"}
      .mkString("(", ", ", ")")
  }

}