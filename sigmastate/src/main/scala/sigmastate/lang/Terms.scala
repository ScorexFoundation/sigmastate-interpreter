package sigmastate.lang

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import scalan.Nullable
import sigmastate.SCollection.{SByteArray, SIntArray}
import sigmastate.Values._
import sigmastate.utils.Overloading.Overload1
import sigmastate._
import sigmastate.serialization.OpCodes
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.lang.TransformingSigmaBuilder._

import scala.language.implicitConversions

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
    override def opType: SFunc = SFunc(SSigmaProp, SBoolean)
  }
  object ZKProofBlock extends ValueCompanion {
    override def opCode: OpCode = OpCodes.Undefined
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
  }

  /** Frontend node to represent variable names parsed in a source code.
    * Should be resolved during compilation to lambda argument, Val definition or
    * compilation environment value. */
  case class Ident(name: String, tpe: SType = NoType) extends Value[SType] {
    override def companion = Ident
    override def opType: SFunc = SFunc(Vector(), tpe)
  }
  object Ident extends ValueCompanion {
    override def opCode: OpCode = OpCodes.Undefined
    def apply(name: String): Ident = Ident(name, NoType)
  }

  // TODO HF: move to sigmastate.Values
  case class Apply(func: Value[SType], args: IndexedSeq[Value[SType]]) extends Value[SType] {
    override def companion = Apply
    override lazy val tpe: SType = func.tpe match {
      case SFunc(_, r, _) => r
      case tColl: SCollectionType[_] => tColl.elemType
      case _ => NoType
    }
    override def opType: SFunc = SFunc(Vector(func.tpe +: args.map(_.tpe):_*), tpe)
  }
  object Apply extends ValueCompanion {
    override def opCode: OpCode = OpCodes.FuncApplyCode
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
  }

  /** Represents in ErgoTree an invocation of method of the object `obj` with arguments `args`.
    * The SMethod instances in STypeCompanions may have type STypeIdent in methods types,
    * but valid ErgoTree should have SMethod instances specialized for specific types of
    * obj and args using `specializeFor`.
    * This means, if we save typeId, mathodId, and we save all the arguments,
    * we can restore the specialized SMethod instance.
    * This work by induction, if we assume all arguments are monomorphic,
    * then we can make MethodCall monomorphic.
    * Thus, all ErgoTree instances are monomorphic by construction.
    *
    * @param obj object on which method will be invoked
    * @param method method to be invoked
    * @param args arguments passed to the method on invocation
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
  }
  object MethodCall extends ValueCompanion {
    override def opCode: OpCode = OpCodes.MethodCallCode

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
  object PropertyCall extends ValueCompanion {
    override def opCode: OpCode = OpCodes.PropertyCallCode
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
    def apply(args: IndexedSeq[(String,SType)], resTpe: SType, body: Value[SType]): Lambda =
      Lambda(Nil, args, resTpe, Some(body))
    def apply(args: IndexedSeq[(String,SType)], resTpe: SType, body: Option[Value[SType]]): Lambda =
      Lambda(Nil, args, resTpe, body)
    def apply(args: IndexedSeq[(String,SType)], body: Value[SType]): Lambda = Lambda(Nil, args, NoType, Some(body))
  }

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
      rewrite(everywherebu(rule[SValue] {
        case node if node != null && node.sourceContext.isEmpty =>
          node.withSrcCtx(srcCtx)
      }))(v).asValue[T]
    }

  }
}
