package sigmastate.lang

import org.ergoplatform.ErgoBox
import sigma.ast
import sigma.ast._
import sigma.ast.SigmaPredef.PredefinedFuncRegistry
import sigma.ast.Value.Typed
import sigma.ast.syntax.{SValue, ValueOps}
import sigma.data.Nullable
import sigma.exceptions.{CompilerException, GraphBuildingException}
import sigma.kiama.rewriting.Rewriter
import sigma.kiama.rewriting.Rewriter.{Duplicator, everywherebu, strategy}
import sigmastate.interpreter.Interpreter.ScriptEnv

/**
  * Type inference and analysis for Sigma expressions.
  */
class DirectCompiler(
  settings: CompilerSettings,
  predefFuncRegistry: PredefinedFuncRegistry
) {
  /** Constructs an instance for the given network type and with default settings. */
  def this(networkPrefix: Byte, predefFuncRegistry: PredefinedFuncRegistry) = this(
    CompilerSettings(networkPrefix, TransformingSigmaBuilder, lowerMethodCalls = true),
    predefFuncRegistry
  )

  import settings.builder._

  def error(msg: String, srcCtx: Option[SourceContext]) = throw new CompilerException(msg, srcCtx)
  def error(msg: String, srcCtx: Nullable[SourceContext]): Nothing = error(msg, srcCtx.toOption)

  def compileNode(env: ScriptEnv, node: SValue): SValue = {
    def recurse(node: SValue): SValue = compileNode(env, node)

    node match {

      case Upcast(Constant(value, _), toTpe: SNumericType) =>
        mkConstant(toTpe.upcast(value.asInstanceOf[AnyVal]), toTpe)

      case Downcast(Constant(value, _), toTpe: SNumericType) =>
        mkConstant(toTpe.downcast(value.asInstanceOf[AnyVal]), toTpe)

      // Rule: col.size --> SizeOf(col)
      case Select(obj_, "size", _) =>
        val obj = recurse(obj_)
        if (obj.tpe.isCollectionLike)
          mkSizeOf(obj.asValue[SCollection[SType]])
        else
          error(s"The type of $obj is expected to be Collection to select 'size' property", obj.sourceContext)

      // Rule: proof.isProven --> IsValid(proof)
      case Select(p, SSigmaPropMethods.IsProven, _) if p.tpe == SSigmaProp =>
        SigmaPropIsProven(recurse(p).asSigmaProp)

      // Rule: prop.propBytes --> SigmaProofBytes(prop)
      case Select(p, SSigmaPropMethods.PropBytes, _) if p.tpe == SSigmaProp =>
        SigmaPropBytes(recurse(p).asSigmaProp)

      // box.R$i[valType] =>
      case sel @ Select(Typed(box, SBox), regName, Some(SOption(valType))) if regName.startsWith("R") =>
        val reg = ErgoBox.registerByName.getOrElse(regName,
          error(s"Invalid register name $regName in expression $sel", sel.sourceContext.toOption))
        mkExtractRegisterAs(recurse(box).asBox, reg, SOption(valType)).asValue[SOption[valType.type]]

      case sel @ Select(obj, field, _) if obj.tpe == SBox =>
        (recurse(obj).asValue[SBox.type], field) match {
          case (box, SBoxMethods.Value) => mkExtractAmount(box)
          case (box, SBoxMethods.PropositionBytes) => mkExtractScriptBytes(box)
          case (box, SBoxMethods.Id) => mkExtractId(box)
          case (box, SBoxMethods.Bytes) => mkExtractBytes(box)
          case (box, SBoxMethods.BytesWithoutRef) => mkExtractBytesWithNoRef(box)
          case (box, SBoxMethods.CreationInfo) => mkExtractCreationInfo(box)
          case _ => error(s"Invalid access to Box property in $sel: field $field is not found", sel.sourceContext)
        }

      case Select(tuple, fn, _) if tuple.tpe.isTuple && fn.startsWith("_") =>
        val index = fn.substring(1).toByte
        mkSelectField(recurse(tuple).asTuple, index)

      case Select(obj, method, Some(tRes: SNumericType))
          if obj.tpe.isNumType && SNumericTypeMethods.isCastMethod(method) =>

        val numValue = recurse(obj).asNumValue
        if (numValue.tpe == tRes)
          numValue
        else if ((numValue.tpe max tRes) == numValue.tpe)
          mkDowncast(numValue, tRes)
        else
          mkUpcast(numValue, tRes)

      case sigma.ast.Apply(col, Seq(index)) if col.tpe.isCollection =>
        mkByIndex(recurse(col).asCollection[SType], recurse(index).asValue[SInt.type], None)

      case _ =>
        def compileChild(env: ScriptEnv, child: Any): AnyRef = child match {
          case child: SValue =>
            compileNode(env, child)
          case xs: Seq[_] =>
            xs.map(compileChild(env, _))
          case p: Product =>
            compileProduct(env, p).asInstanceOf[AnyRef]
          case x => x.asInstanceOf[AnyRef] // everything else is left as is (modulo boxing)
        }

        def compileProduct[T <: Product](env: ScriptEnv, node: T): T = {
          val children = node.productIterator
            .map(compileChild(env, _))
            .toArray
          Duplicator(node, children)
        }

        compileProduct(env, node)
    }
  }
}
