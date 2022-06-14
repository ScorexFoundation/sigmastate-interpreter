package sigmastate

import Values._
import org.ergoplatform.{Context, Global, Inputs, Height, LastBlockUtxoRootHash, MinerPubkey, Outputs, Self}
import org.typelevel.paiges.Doc
import sigmastate.serialization.OpCodes
import sigmastate.utxo._
import sigmastate.lang.Terms.{Apply, ApplyTypes, Block, Ident, Lambda, MethodCall, MethodCallLike, Select, ValNode}

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
      Doc.char('{') + Doc.space + argsWithTypesDoc(args) + Doc.text(" =>") + Doc.line +
        createDoc(body).indent(i) + Doc.line +
        Doc.char('}')
    case BlockValue(items, result) =>
      val prettyItems = items.map(item => createDoc(item))
      Doc.intercalate(Doc.line, prettyItems)+ Doc.line +
        createDoc(result)
    case SomeValue(_) | NoneValue(_) => ??? // Not implemented in ErgoTree (as of v5.0)

    // ErgoLike
    case Height => Doc.text("HEIGHT")
    case Inputs => Doc.text("INPUTS")
    case Outputs => Doc.text("OUTPUTS")
    case Self => Doc.text("SELF")
    case Context => Doc.text("CONTEXT")
    case Global => Doc.text("Global")
    // TODO: Not possible to reach following 2 cases until https://github.com/ScorexFoundation/sigmastate-interpreter/issues/799
    case MinerPubkey => Doc.text("minerPubKey")
    case LastBlockUtxoRootHash => Doc.text("LastBlockUtxoRootHash") 

    // Transformers
    case MapCollection(input, mapper) => createDoc(input) + Doc.text(".map(") + createDoc(mapper) + Doc.char(')')
    case Append(coll1, coll2) => createDoc(coll1) + Doc.text(".append") + wrapWithParens(createDoc(coll2))
    case Slice(input, from, until) => createDoc(input) + Doc.text(".slice") + nTupleDoc(List(from, until).map(createDoc))
    case Filter(input, condition) => createDoc(input) + Doc.text(".filter") + wrapWithParens(createDoc(condition))
    case bt: BooleanTransformer[_] => bt match {
      case Exists(input, condition) => createDoc(input) + Doc.text(".exists") + wrapWithParens(createDoc(condition))
      case ForAll(input, condition) => createDoc(input) + Doc.text(".forall") + wrapWithParens(createDoc(condition))
    }
    case Fold(input, zero, foldOp) => createDoc(input) + Doc.text(".fold") + nTupleDoc(List(zero, foldOp).map(createDoc))
    case SelectField(input, idx) => createDoc(input) + Doc.text(s"._$idx")
    case ByIndex(input, index, defaultIndex) =>
      val body = defaultIndex match {
        case Some(v) => Doc.text(".getOrElse") + nTupleDoc(List(index, v).map(createDoc))
        case None => wrapWithParens(createDoc(index))
      }
      createDoc(input) + body
    // TODO: Not used in final ErgoTree representation.
    case SigmaPropIsProven(input) => ???
    case SigmaPropBytes(input) => createDoc(input) + Doc.text(".propBytes")
    case SizeOf(input) => createDoc(input) + Doc.text(".size")
    case ExtractAmount(input) => createDoc(input) + Doc.text(".value")
    case ExtractScriptBytes(input) => createDoc(input) + Doc.text(".propositionBytes")
    case ExtractBytes(input) => createDoc(input) + Doc.text(".bytes")
    case ExtractBytesWithNoRef(input) => createDoc(input) + Doc.text(".bytesWithoutRef")
    case ExtractId(input) => createDoc(input) + Doc.text(".id")
    // TODO: Check what is returned inside elemType when maybeTpe is None?
    case ExtractRegisterAs(input, registerId, maybeTpe) =>
      createDoc(input) + Doc.text(s".$registerId") + wrapWithBrackets(STypeDoc(maybeTpe.elemType))
    case ExtractCreationInfo(input) => createDoc(input) + Doc.text(".creationInfo")
    // TODO: Can be part of final ErgoTree?
    case d: Deserialize[_] => d match {
      case DeserializeContext(id, tpe) => ???
      case DeserializeRegister(reg, tpe, default) => ???
    }
    // TODO: Check how to handle None inside maybeTpe
    case GetVar(varId, maybeTpe) =>
      Doc.text("getVar") + wrapWithBrackets(STypeDoc(maybeTpe.elemType)) + wrapWithParens(createDoc(varId))
    case OptionGet(input) => createDoc(input) + Doc.text(".get")
    case OptionGetOrElse(input, default) => createDoc(input) + Doc.text(".getOrElse") + wrapWithParens(createDoc(default))
    case OptionIsDefined(x) => createDoc(x) + Doc.text(".isDefined")

    // Terms
    // Following nodes are not part of final ErgoTree
    case Block(_, _) => ???
    case ValNode(_, _, _) => ???
    case Select(_, _, _) => ???
    case Ident(_, _) => ???
    case ApplyTypes(_, _) => ???
    case MethodCallLike(_, _, _, _) => ???
    case Lambda(_, _, _, _) => ???

    case Apply(func, args) => createDoc(func) + nTupleDoc(args.map(createDoc))
    case MethodCall(obj, method, args, map) =>
      val argsDoc = if (args.nonEmpty) nTupleDoc(args.map(createDoc)) else Doc.empty
      createDoc(obj) + Doc.char('.') + Doc.text(method.name) + argsDoc

    // Trees
    case BoolToSigmaProp(value) => Doc.text("sigmaProp") + wrapWithParens(createDoc(value))
    case CreateProveDlog(value) => Doc.text("proveDlog") + wrapWithParens(createDoc(value))
    // TODO: Can be removed as it isn't used anymore.
    case CreateAvlTree(_, _, _, _) => ???
    case CreateProveDHTuple(gv, hv, uv, vv) => Doc.text("proveDHTuple") + nTupleDoc(List(gv, hv, uv, vv).map(createDoc))
    case st: SigmaTransformer[_, _] => st match {
      // TODO: Test with more than 2 elements inside items
      case SigmaAnd(items) => Doc.intercalate(Doc.text(" && "), items.map(createDoc))
      case SigmaOr(items) => Doc.intercalate(Doc.text(" || "), items.map(createDoc))
    }
    case OR(input) => Doc.text("anyOf") + wrapWithParens(createDoc(input))
    case XorOf(input) => Doc.text("xorOf") + wrapWithParens(createDoc(input))
    case AND(input) => Doc.text("allOf") + wrapWithParens(createDoc(input))
    case AtLeast(bound, input) => Doc.text("atLeast") + nTupleDoc(List(bound, input).map(createDoc))
    case Upcast(input, tpe) => createDoc(input) + Doc.text(".to") + STypeDoc(tpe)
    case Downcast(input, tpe) => createDoc(input) + Doc.text(".to") + STypeDoc(tpe)
    case LongToByteArray(input) => Doc.text("longToByteArray") + wrapWithParens(createDoc(input))
    case ByteArrayToLong(input) => Doc.text("byteArrayToLong") + wrapWithParens(createDoc(input))
    case ByteArrayToBigInt(input) => Doc.text("byteArrayToBigInt") + wrapWithParens(createDoc(input))
    case nr: NotReadyValueGroupElement => nr match {
      case DecodePoint(input) => Doc.text("decodePoint") + wrapWithParens(createDoc(input))
      case Exponentiate(l, r) => createDoc(l) + Doc.text(".exp") + wrapWithParens(createDoc(r))
      case MultiplyGroup(l, r) => createDoc(l) + Doc.text(".multiply") + wrapWithParens(createDoc(r))
    }
    case ch: CalcHash => ch match {
      case CalcBlake2b256(input) => Doc.text("blake2b256") + wrapWithParens(createDoc(input))
      case CalcSha256(input) => Doc.text("sha256") + wrapWithParens(createDoc(input))
    }
    case SubstConstants(scriptBytes, positions, newValues) =>
      val body = Doc.intercalate(Doc.comma + Doc.space, List(createDoc(scriptBytes), createDoc(positions), createDoc(newValues)))
      Doc.text("substConstants") + wrapWithParens(body)

    case t: Triple[_, _, _] => t match {
      case ArithOp(l, r, OpCodes.PlusCode) => createDoc(l) + Doc.text(" + ") + createDoc(r)
      case ArithOp(l, r, OpCodes.MinusCode) => createDoc(l) + Doc.text(" - ") + createDoc(r)
      case ArithOp(l, r, OpCodes.MultiplyCode) => createDoc(l) + Doc.text(" * ") + createDoc(r)
      case ArithOp(l, r, OpCodes.DivisionCode) => createDoc(l) + Doc.text(" / ") + createDoc(r)
      case ArithOp(l, r, OpCodes.ModuloCode) => createDoc(l) + Doc.text(" % ") + createDoc(r)
      // TODO: min/max usage in script code
      case ArithOp(l, r, OpCodes.MinCode) => ???
      case ArithOp(l, r, OpCodes.MaxCode) => ???
      case ArithOp(l, r, _) => ???
      // TODO: Implement after https://github.com/ScorexFoundation/sigmastate-interpreter/issues/474
      case BitOp(l, r, OpCodes.BitOrCode) => binaryOperationWithParens(createDoc(l), createDoc(r), Doc.char('|'))
      case BitOp(l, r, OpCodes.BitAndCode) => binaryOperationWithParens(createDoc(l), createDoc(r), Doc.char('&'))
      // Possible xor clash with BinXor?
      case BitOp(_, _, _) => ???
      case Xor(l, r) => Doc.text("xor") + nTupleDoc(List(l, r).map(createDoc))
      case sr: SimpleRelation[_] => sr match {
        case GT(l, r) => binaryOperationWithParens(createDoc(l), createDoc(r), Doc.text(" > "))
        case GE(l, r) => binaryOperationWithParens(createDoc(l), createDoc(r), Doc.text(" >= "))
        case EQ(l, r) => binaryOperationWithParens(createDoc(l), createDoc(r), Doc.text(" == "))
        case NEQ(l, r) => binaryOperationWithParens(createDoc(l), createDoc(r), Doc.text(" != "))
        case LT(l, r) => binaryOperationWithParens(createDoc(l), createDoc(r), Doc.text(" < "))
        case LE(l, r) => binaryOperationWithParens(createDoc(l), createDoc(r), Doc.text(" <= "))
      }
      case BinOr(l, r) => binaryOperationWithParens(createDoc(l), createDoc(r), Doc.text(" || "))
      case BinAnd(l, r) => binaryOperationWithParens(createDoc(l), createDoc(r), Doc.text(" && "))
      case BinXor(l, r) => binaryOperationWithParens(createDoc(l), createDoc(r), Doc.text(" ^ "))
    }
    case oneArgOp: OneArgumentOperation[_, _] => oneArgOp match {
      // TODO: Only used in parser?
      case BitInversion(input) => Doc.char('~') + createDoc(input)
      case Negation(input) => Doc.char('!') + createDoc(input)
    }
    // TODO: How are ModQs used in ergo script?
    case ModQ(input) => ???
    case ModQArithOp(l, r, opCode) => ???

    case quad: Quadruple[_, _, _, _] => quad match {
      case If(condition, trueBranch, falseBranch) =>
        Doc.text("if (") + createDoc(condition) + Doc.text(") {") + Doc.line +
        createDoc(trueBranch).indent(i) + Doc.line +
        Doc.text("} else {") + Doc.line +
        createDoc(falseBranch).indent(i) + Doc.line +
        Doc.char('}')
      // TODO: How it can be used in ergo script?
      case TreeLookup(tree, key, proof) => ???
    }
    case LogicalNot(input) => Doc.char('!') + createDoc(input)
  }

  private def binaryOperationWithParens(l: Doc, r: Doc, sep: Doc) = wrapWithParens(l) + sep + wrapWithParens(r)
  private def wrapWithParens(d: Doc): Doc = d.tightBracketBy(Doc.char('('), Doc.char(')'))
  private def wrapWithBrackets(d: Doc): Doc = d.tightBracketBy(Doc.char('['), Doc.char(']'))

  /* Create argument representation enclosed in brackets with types, e.g. `($5: String, $1: Int)` */
  private def argsWithTypesDoc(args: Seq[(Int, SType)]): Doc = {
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
    case SSigmaProp => Doc.text("SigmaProp")
    case SCollectionType(elemType) => Doc.text("Coll") + wrapWithBrackets(STypeDoc(elemType))
    case STuple(items) => nTupleDoc(items.map(STypeDoc))
    case SContext => Doc.text("Context")
    case SOption(elemType) => Doc.text("Option") + wrapWithBrackets(STypeDoc(elemType))
    case SGroupElement => Doc.text("GroupElement")
    case SPreHeader => Doc.text("PreHeader")
    // TODO: Not tested
    case NoType => Doc.empty
    case SString => Doc.text("String")
    case SAny => Doc.text("Any")
    case SUnit => Doc.text("Unit")
    case SFunc(tDom, tRange, tpeParams) => nTupleDoc(tDom.map(STypeDoc)) + Doc.text(" => ") + STypeDoc(tRange)
    case SAvlTree => Doc.text("AvlTree")
    case SHeader => Doc.text("Header")
    case SGlobal => Doc.text("Global")
    // Not used in final ergo tree
    case STypeApply(name, args) => ???
    case STypeVar(name) => ???
  }

  private def nTupleDoc(items: Seq[Doc]): Doc =
    wrapWithParens(
      Doc.intercalate(
        Doc.comma + Doc.space,
        items))

}