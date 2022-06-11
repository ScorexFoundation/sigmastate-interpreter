package sigmastate

import Values._
import org.ergoplatform.{Global, Outputs, Self}
import org.typelevel.paiges.Doc
import sigmastate.serialization.OpCodes
import sigmastate.utxo.{Exists, ExtractAmount, ExtractRegisterAs, OptionGet, OptionIsDefined, SelectField}
import sigmastate.lang.Terms.MethodCall

/**
 * TODO: Docs - arguments/values naming $i
 * Brainstorm ideas: create configuration for printer (explicit types, curly/normal brackets, `to{$tpe}` explicit casting)
 */
object PrettyPrintErgoTree {

  def prettyPrint(t: SValue, indentation: Int = 2, width: Int = 80): String = createDoc(t)(indentation).render(width)

  private def createDoc(t: SValue)(implicit i: Int): Doc = t match {
    // Values
    case ev: EvaluatedValue[SType] => ev match {
      case c: Constant[SType] => c match {
        case FalseLeaf => Doc.text("false")
        case TrueLeaf => Doc.text("true")
        case ConstantNode(value, tpe) => Doc.text(s"$value.to") + STypeDoc(tpe)
      }
      case UnitConstant() => ??? // TODO: Only used in parser?
      case GroupGenerator => ??? // TODO: What is that?
      case ec: EvaluatedCollection[_, _] => ec match {
        case ConcreteCollection(items, elemType) =>
          Doc.text(s"Coll[") + STypeDoc(elemType) + Doc.char(']') +
          wrapWithParens(Doc.intercalate(Doc.comma + Doc.space, items.map(createDoc)))
      }
    }
    case bi: BlockItem => bi match {
      case ValDef(id, _, rhs) => Doc.text(s"val $$$id = ") + createDoc(rhs)
    } 
    case Tuple(items) => nTupleDoc(items.map(createDoc))
    case ValUse(id, tpe) => Doc.text(s"$$$id")
    case ConstantPlaceholder(id, tpe) => ??? // TODO: See unfinished test
    case TaggedVariableNode(varId, tpe) => ??? // TODO: Does not make sense for printer?
    case FalseSigmaProp | TrueSigmaProp => ??? // TODO: Does not make sense for printer?
    case FuncValue(args, body) =>
      Doc.char('{') + Doc.space + argsDoc(args) + Doc.text(" =>") + Doc.line +
        createDoc(body).indent(i) + Doc.line +
        Doc.char('}')
    case BlockValue(items, result) =>
      val prettyItems = items.map(item => createDoc(item))
      Doc.intercalate(Doc.line, prettyItems)+ Doc.line +
        createDoc(result)
    case SomeValue(_) | NoneValue(_) => ??? // Not implemented in ErgoTree (as of v5.0)

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
    case BinOr(l, r) => binaryOperationWithParens(createDoc(l), createDoc(r), Doc.text(" || "))
    case BinAnd(l, r) => binaryOperationWithParens(createDoc(l), createDoc(r), Doc.text(" && "))
    case BinXor(l, r) => binaryOperationWithParens(createDoc(l), createDoc(r), Doc.text(" ^ "))
    case ArithOp(left, right, OpCodes.PlusCode) => createDoc(left) + Doc.text(" + ") + createDoc(right)
    case ArithOp(left, right, OpCodes.MinusCode) => createDoc(left) + Doc.text(" - ") + createDoc(right)
    case ArithOp(left, right, OpCodes.MultiplyCode) => createDoc(left) + Doc.text(" * ") + createDoc(right)
    case ArithOp(left, right, OpCodes.DivisionCode) => createDoc(left) + Doc.text(" / ") + createDoc(right)
    case ArithOp(left, right, OpCodes.ModuloCode) => createDoc(left) + Doc.text(" % ") + createDoc(right)
    case GT(l, r) => binaryOperationWithParens(createDoc(l), createDoc(r), Doc.text(" > "))
    case EQ(l, r) => binaryOperationWithParens(createDoc(l), createDoc(r), Doc.text(" == "))
    case SubstConstants(scriptBytes, positions, newValues) =>
      val body = Doc.intercalate(Doc.comma + Doc.space, List(createDoc(scriptBytes), createDoc(positions), createDoc(newValues)))
      Doc.text("substConstants") + wrapWithParens(body)
    case BoolToSigmaProp(value) => Doc.text("sigmaProp") + wrapWithParens(createDoc(value))
    // TODO: not covered by test
    case LT(left, right) => wrapWithParens(createDoc(left)) + Doc.text(" < ") + wrapWithParens(createDoc(right))
    case nr: NotReadyValueGroupElement => nr match {
      case DecodePoint(input) => ???
      case Exponentiate(left, right) => ???
      case MultiplyGroup(left, right) => ???
    }
  }

  private def binaryOperationWithParens(l: Doc, r: Doc, sep: Doc) = wrapWithParens(l) + sep + wrapWithParens(r)
  private def wrapWithParens(d: Doc): Doc = d.tightBracketBy(Doc.char('('), Doc.char(')'))

  /* Create argument representation enclosed in brackets with types, e.g. `($5: String, $1: Int)` */
  private def argsDoc(args: Seq[(Int, SType)]): Doc = {
    val argsWithTypes = args.map { case (i, tpe) => Doc.text(s"$$$i:") + Doc.space + STypeDoc(tpe) }
    nTupleDoc(argsWithTypes)
  }

  private def STypeDoc(tpe: SType): Doc = tpe match {
    case SBoolean => Doc.text("Boolean")
    case SByte => Doc.text("Byte")
    case SShort => Doc.text("Short")
    case SInt => Doc.text("Int")
    case SLong => Doc.text("Long")
    case SBigInt => Doc.text("BigInt")
    case SBox => Doc.text("Box")
    // TODO: Shouldn't be Boolean?
    case SSigmaProp => Doc.text("Any")
    case SCollectionType(elemType) => Doc.text("Coll[") + STypeDoc(elemType) + Doc.char(']')
    case STuple(items) => nTupleDoc(items.map(STypeDoc))
  }

  private def nTupleDoc(items: Seq[Doc]): Doc =
    Doc
      .intercalate(Doc.comma + Doc.space, items)
      .tightBracketBy(Doc.char('('), Doc.char(')'))

}