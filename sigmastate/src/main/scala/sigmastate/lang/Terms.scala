package sigmastate.lang

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import scalan.Nullable
import sigmastate.SCollection.{SIntArray, SByteArray}
import sigmastate.Values._
import sigmastate.utils.Overloading.Overload1
import sigmastate._
import sigmastate.interpreter.{Interpreter, ErgoTreeEvaluator}
import sigmastate.interpreter.ErgoTreeEvaluator.DataEnv
import sigmastate.serialization.OpCodes
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.lang.TransformingSigmaBuilder._

import scala.language.implicitConversions
import scala.collection.mutable.WrappedArray
import spire.syntax.all.cfor

object Terms {

  /** Frontend representation of a block of Val definitions.
    * { val x = ...; val y = ... }
    * This node is not part of ErgoTree and hence have Undefined opCode. */
  case class Block(bindings: Seq[Val], result: SValue) extends Value[SType] {
    override def companion = Block
    override def tpe: SType = result.tpe

    /** This is not used as operation, but rather to form a program structure */
    override def opType: SFunc = Value.notSupportedError(this, "opType")
  }
  object Block extends ValueCompanion {
    override def opCode: OpCode = OpCodes.Undefined
    override def costKind: CostKind = Value.notSupportedError(this, "costKind")
    def apply(let: Val, result: SValue)(implicit o1: Overload1): Block =
      Block(Seq(let), result)
  }

  /** IR node to represent explicit Zero Knowledge scope in ErgoTree.
    * Compiler checks Zero Knowledge properties and issue error message is case of violations.
    * ZK-scoping is optional, it can be used when the user want to ensure Zero Knowledge of
    * specific set of operations.
    * Usually it will require simple restructuring of the code to make the scope body explicit.
    * Invariants checked by the compiler:
    *  - single ZKProof in ErgoTree in a root position
    *  - no boolean operations in the body, because otherwise the result may be disclosed
    *  - all the operations are over SigmaProp values
    *
    * For motivation and details see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/236
    * */
  case class ZKProofBlock(body: SigmaPropValue) extends BoolValue {
    override def companion = ZKProofBlock
    override def tpe = SBoolean
    override def opType: SFunc = ZKProofBlock.OpType
  }
  object ZKProofBlock extends ValueCompanion {
    override def opCode: OpCode = OpCodes.Undefined
    override def costKind: CostKind = Value.notSupportedError(this, "costKind")
    val OpType = SFunc(SSigmaProp, SBoolean)
  }

  trait Val extends Value[SType] {
    val name: String
    val givenType: SType
    val body: SValue
  }
  object Val {
    def apply(name: String, body: SValue): Val = ValNode(name, NoType, body)
    def apply(name: String, givenType: SType, body: SValue): Val = ValNode(name, givenType, body)
    def unapply(v: SValue): Option[(String, SType, SValue)] = v match {
      case ValNode(name, givenType, body) => Some((name, givenType, body))
      case _ => None
    }
  }

  case class ValNode(name: String,
                     givenType: SType,
                     body: SValue) extends Val {
    override def companion = ValNode
    override def tpe: SType = givenType ?: body.tpe
    /** This is not used as operation, but rather to form a program structure */
    override def opType: SFunc = Value.notSupportedError(this, "opType")
  }
  object ValNode extends ValueCompanion {
    override def opCode: OpCode = OpCodes.Undefined
    override def costKind: CostKind = Value.notSupportedError(this, "costKind")
  }

  /** Frontend node to select a field from an object. Should be transformed to SelectField*/
  case class Select(obj: Value[SType], field: String, resType: Option[SType] = None) extends Value[SType] {
    override def companion = Select
    override val tpe: SType = resType.getOrElse(obj.tpe match {
      case p: SProduct =>
        val i = p.methodIndex(field)
        if (i == -1) NoType
        else p.methods(i).stype
      case _ => NoType
    })
    override def opType: SFunc = SFunc(obj.tpe, tpe)
  }
  object Select extends ValueCompanion {
    override def opCode: OpCode = OpCodes.Undefined
    override def costKind: CostKind = Value.notSupportedError(this, "costKind")
  }

  /** Frontend node to represent variable names parsed in a source code.
    * Should be resolved during compilation to lambda argument, Val definition or
    * compilation environment value. */
  case class Ident(name: String, tpe: SType = NoType) extends Value[SType] {
    override def companion = Ident
    override def opType: SFunc = SFunc(WrappedArray.empty, tpe)
  }
  object Ident extends ValueCompanion {
    override def opCode: OpCode = OpCodes.Undefined
    override def costKind: CostKind = Value.notSupportedError(this, "costKind")
    def apply(name: String): Ident = Ident(name, NoType)
  }

  // TODO refactor: move to sigmastate.Values
  /** ErgoTree node which represents application of function `func` to the given arguments.
    * @param func expression which evaluates to a function
    * @param args arguments of the function application
    */
  case class Apply(func: Value[SType], args: IndexedSeq[Value[SType]]) extends Value[SType] {
    override def companion = Apply
    override lazy val tpe: SType = func.tpe match {
      case SFunc(_, r, _) => r
      case tColl: SCollectionType[_] => tColl.elemType
      case _ => NoType
    }
    override lazy val opType: SFunc = {
      val nArgs = args.length
      val argTypes = new Array[SType](nArgs + 1)
      argTypes(0) = func.tpe
      cfor(0)(_ < nArgs, _ + 1) { i =>
        argTypes(i + 1) = args(i).tpe
      }
      SFunc(argTypes, tpe)
    }

    protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
      addCost(Apply.costKind)
      if (args.length == 1) {
        val fV = func.evalTo[Any => Any](env)
        val argV = args(0).evalTo[Any](env)
        fV(argV)
      } else {
        // zero or more than 1 argument functions are not supported in v4.x, v5.0
        // see `case Terms.Apply(f, Seq(x))` in RuntimeCosting which means other cases are not supported.
        Interpreter.error(s"Function application must have 1 argument, but was: $this")
      }
    }
  }
  object Apply extends FixedCostValueCompanion {
    override def opCode: OpCode = OpCodes.FuncApplyCode
    /** Cost of: 1) switch on the number of args 2) Scala method call 3) add args to env
      * Old cost: lambdaInvoke == 30 */
    override val costKind = FixedCost(JitCost(30))
  }

  /** Apply types for type parameters of input value. */
  case class ApplyTypes(input: Value[SType], tpeArgs: Seq[SType]) extends Value[SType] { node =>
    override def companion = ApplyTypes
    override lazy val tpe: SType = input.tpe match {
      case funcType: SFunc =>
        val subst = funcType.tpeParams.map(_.ident).zip(tpeArgs).toMap
        SigmaTyper.applySubst(input.tpe, subst)
      case _ => input.tpe
    }
    /** This is not used as operation, but rather to form a program structure */
    override def opType: SFunc = Value.notSupportedError(this, "opType")
  }
  object ApplyTypes extends ValueCompanion {
    override def opCode: OpCode = OpCodes.Undefined
    override def costKind: CostKind = Value.notSupportedError(this, "costKind")
  }

  /** Frontend node to represent potential method call in a source code.
    * Should be resolved during compilation to MethodCall.
    * Cannot be serialized to ErgoTree. */
  case class MethodCallLike(obj: Value[SType], name: String, args: IndexedSeq[Value[SType]], tpe: SType = NoType) extends Value[SType] {
    override def companion = MethodCallLike
    override def opType: SFunc = SFunc(obj.tpe +: args.map(_.tpe), tpe)
  }
  object MethodCallLike extends ValueCompanion {
    override def opCode: OpCode = OpCodes.Undefined
    override def costKind: CostKind = Value.notSupportedError(this, "costKind")
  }

  /** Represents in ErgoTree an invocation of method of the object `obj` with arguments `args`.
    * The SMethod instances in STypeCompanions may have type STypeIdent in methods types,
    * but valid ErgoTree should have SMethod instances specialized for specific types of
    * obj and args using `specializeFor`.
    * This means, if we save typeId, methodId, and we save all the arguments,
    * we can restore the specialized SMethod instance.
    * This work by induction, if we assume all arguments are monomorphic,
    * then we can make MethodCall monomorphic.
    * Thus, all ErgoTree instances are monomorphic by construction.
    *
    * @param obj       object on which method will be invoked
    * @param method    method to be invoked
    * @param args      arguments passed to the method on invocation
    * @param typeSubst a map of concrete type for each generic type parameter
    */
  case class MethodCall(obj: Value[SType],
                        method: SMethod,
                        args: IndexedSeq[Value[SType]],
                        typeSubst: Map[STypeVar, SType]) extends Value[SType] {
    override def companion = if (args.isEmpty) PropertyCall else MethodCall
    override def opType: SFunc = SFunc(obj.tpe +: args.map(_.tpe), tpe)
    override val tpe: SType = method.stype match {
      case f: SFunc => f.tRange.withSubstTypes(typeSubst)
      case t => t.withSubstTypes(typeSubst)
    }

    /** @hotspot don't beautify this code */
    protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
      val objV = obj.evalTo[Any](env)
      addCost(MethodCall.costKind) // MethodCall overhead
      method.costKind match {
        case fixed: FixedCost =>
          val extra = method.extraDescriptors
          val extraLen = extra.length
          val len = args.length
          val argsBuf = new Array[Any](len + extraLen)
          cfor(0)(_ < len, _ + 1) { i =>
            argsBuf(i) = args(i).evalTo[Any](env)
          }
          cfor(0)(_ < extraLen, _ + 1) { i =>
            argsBuf(len + i) = extra(i)
          }
          var res: Any = null
          E.addFixedCost(fixed, method.opDesc) {
            res = method.invokeFixed(objV, argsBuf)
          }
          res
        case _ =>
          val len = args.length
          val argsBuf = new Array[Any](len + 3)
          argsBuf(0) = this
          argsBuf(1) = objV
          cfor(0)(_ < len, _ + 1) { i =>
            argsBuf(i + 2) = args(i).evalTo[Any](env)
          }
          argsBuf(argsBuf.length - 1) = E

          val evalMethod = method.genericMethod.evalMethod
          evalMethod.invoke(method.objType, argsBuf.asInstanceOf[Array[AnyRef]]:_*)
      }
    }
  }

  object MethodCall extends FixedCostValueCompanion {
    override def opCode: OpCode = OpCodes.MethodCallCode
    /** Cost of: 1) packing args into Array 2) java.lang.reflect.Method.invoke */
    override val costKind = FixedCost(JitCost(4))

    /** Helper constructor which allows to cast the resulting node to the specified
      * [[sigmastate.Values.Value]] type `T`.
      * @see [[sigmastate.lang.Terms.MethodCall]]
      */
    def typed[T <: SValue](obj: Value[SType],
                           method: SMethod,
                           args: IndexedSeq[Value[SType]],
                           typeSubst: Map[STypeVar, SType]): T = {
      MethodCall(obj, method, args, typeSubst).asInstanceOf[T]
    }
  }
  object PropertyCall extends FixedCostValueCompanion {
    override def opCode: OpCode = OpCodes.PropertyCallCode
    /** Cost of: 1) packing args into Array 2) java.lang.reflect.Method.invoke */
    override val costKind = FixedCost(JitCost(4))
  }

  case class STypeParam(ident: STypeVar, upperBound: Option[SType] = None, lowerBound: Option[SType] = None) {
    assert(upperBound.isEmpty && lowerBound.isEmpty, s"Type parameters with bounds are not supported, but found $this")
    override def toString = ident.toString + upperBound.fold("")(u => s" <: $u") + lowerBound.fold("")(l => s" >: $l")
  }
  object STypeParam {
    implicit def typeIdentToTypeParam(id: STypeVar): STypeParam = STypeParam(id)
  }

  /** Frontend implementation of lambdas. Should be transformed to FuncValue. */
  case class Lambda(
        tpeParams: Seq[STypeParam],
        args: IndexedSeq[(String,SType)],
        givenResType: SType,
        body: Option[Value[SType]]) extends Value[SFunc]
  {
    require(!(tpeParams.nonEmpty && body.nonEmpty), s"Generic function definitions are not supported, but found $this")
    override def companion = Lambda
    override lazy val tpe: SFunc = {
      val sRange = givenResType ?: body.fold(NoType: SType)(_.tpe)
      SFunc(args.map(_._2), sRange, tpeParams)
    }
    /** This is not used as operation, but rather to form a program structure */
    override def opType: SFunc = SFunc(Vector(), tpe)
  }
  object Lambda extends ValueCompanion {
    override def opCode: OpCode = OpCodes.Undefined
    override def costKind: CostKind = Value.notSupportedError(this, "costKind")
    def apply(args: IndexedSeq[(String,SType)], resTpe: SType, body: Value[SType]): Lambda =
      Lambda(Nil, args, resTpe, Some(body))
    def apply(args: IndexedSeq[(String,SType)], resTpe: SType, body: Option[Value[SType]]): Lambda =
      Lambda(Nil, args, resTpe, body)
    def apply(args: IndexedSeq[(String,SType)], body: Value[SType]): Lambda = Lambda(Nil, args, NoType, Some(body))
  }

  /** Operation identity descriptor used in AOT costing (see usages in RuntimeCosting and
    * CostTable) */
  case class OperationId(name: String, opType: SFunc)

  implicit class ValueOps(val v: Value[SType]) extends AnyVal {
    def asValue[T <: SType]: Value[T] = v.asInstanceOf[Value[T]]
    def asNumValue: Value[SNumericType] = v.asInstanceOf[Value[SNumericType]]
    def asStringValue: Value[SString.type] = v.asInstanceOf[Value[SString.type]]
    def asBoolValue: Value[SBoolean.type] = v.asInstanceOf[Value[SBoolean.type]]
    def asByteValue: Value[SByte.type] = v.asInstanceOf[Value[SByte.type]]
    def asShortValue: Value[SShort.type] = v.asInstanceOf[Value[SShort.type]]
    def asIntValue: Value[SInt.type] = v.asInstanceOf[Value[SInt.type]]
    def asLongValue: Value[SLong.type] = v.asInstanceOf[Value[SLong.type]]
    def asBigInt: Value[SBigInt.type] = v.asInstanceOf[Value[SBigInt.type]]
    def asBox: Value[SBox.type] = v.asInstanceOf[Value[SBox.type]]
    def asGroupElement: Value[SGroupElement.type] = v.asInstanceOf[Value[SGroupElement.type]]
    def asSigmaProp: Value[SSigmaProp.type] = v.asInstanceOf[Value[SSigmaProp.type]]
    def asByteArray: Value[SByteArray] = v.asInstanceOf[Value[SByteArray]]
    def asIntArray: Value[SIntArray] = v.asInstanceOf[Value[SIntArray]]
    def asCollection[T <: SType]: Value[SCollection[T]] = v.asInstanceOf[Value[SCollection[T]]]
    def asOption[T <: SType]: Value[SOption[T]] = v.asInstanceOf[Value[SOption[T]]]
    def asTuple: Value[STuple] = v.asInstanceOf[Value[STuple]]
    def asFunc: Value[SFunc] = v.asInstanceOf[Value[SFunc]]
    def asConcreteCollection[T <: SType]: ConcreteCollection[T] = v.asInstanceOf[ConcreteCollection[T]]
    def upcastTo[T <: SNumericType](targetType: T): Value[T] = {
      assert(v.tpe.isInstanceOf[SNumericType],
        s"Cannot upcast value of type ${v.tpe} to $targetType: only numeric types can be upcasted.")
      val tV = v.asValue[SNumericType]
      assert(targetType.max(tV.tpe) == targetType,
        s"Invalid upcast from $tV to $targetType: target type should be larger than source type.")
      if (targetType == tV.tpe) v.asValue[T]
      else
        mkUpcast(tV, targetType).withSrcCtx(v.sourceContext)
    }
    def withSrcCtx[T <: SType](sourceContext: Nullable[SourceContext]): Value[T] = {
      v.sourceContext = sourceContext
      v.asValue[T]
    }
    /**
      * Set source context only if it's empty
      */
    def withEnsuredSrcCtx[T <: SType](sourceContext: Nullable[SourceContext]): Value[T] = {
      if (v.sourceContext.isEmpty) v.sourceContext = sourceContext
      v.asValue[T]
    }
    /**
      * Set source context to all nodes missing source context in the given tree.
      * @param srcCtx source context to set
      * @return AST where all nodes with missing source context are set to the given srcCtx
      */
    def withPropagatedSrcCtx[T <: SType](srcCtx: Nullable[SourceContext]): Value[T] = {
      rewrite(everywherebu(rule[Any] {
        case node: SValue if node != null && node.sourceContext.isEmpty =>
          node.withSrcCtx(srcCtx)
      }))(v).asValue[T]
    }

  }
}
