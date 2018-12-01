package sigmastate.lang

import org.ergoplatform.Self
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate.utils.Overloading.Overload1
import sigmastate._
import sigmastate.serialization.OpCodes
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.interpreter.Context
import sigmastate.lang.TransformingSigmaBuilder._
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo.{ExtractRegisterAs, SigmaPropIsValid, Slice}

object Terms {

  case class Block(bindings: Seq[Val], result: SValue) extends Value[SType] {
    override val opCode: OpCode = OpCodes.Undefined
    override def evaluated: Boolean = false
    def tpe: SType = result.tpe

    /** This is not used as operation, but rather to form a program structure */
    def opType: SFunc = Value.notSupportedError(this, "opType")
  }
  object Block {
    def apply(let: Val, result: SValue)(implicit o1: Overload1): Block =
      Block(Seq(let), result)
  }

  /** IR node to represent explicit Zero Knowledge scope in ErgoTree.
    * Compiler checks Zero Knowledge properties and issue error message is case of violations.
    * ZK-scoping is optional, at can be used when the user want to ensure Zero Knowledge of
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
    override val opCode: OpCode = OpCodes.Undefined
    override def tpe = SBoolean
    override def evaluated: Boolean = false
    override def opType: SFunc = SFunc(SSigmaProp, SBoolean)
  }

  trait Val extends Value[SType] {
    val name: String
    val givenType: SType
    val body: SValue
  }

  case class ValNode(name: String, givenType: SType, body: SValue) extends Val {
    override val opCode: OpCode = OpCodes.Undefined
    override def evaluated: Boolean = ???
    def tpe: SType = givenType ?: body.tpe
    /** This is not used as operation, but rather to form a program structure */
    def opType: SFunc = Value.notSupportedError(this, "opType")
  }
  object Val {
    def apply(name: String, body: SValue): Val = ValNode(name, NoType, body)
    def apply(name: String, givenType: SType, body: SValue): Val = ValNode(name, givenType, body)
    def unapply(v: SValue): Option[(String, SType, SValue)] = v match {
      case ValNode(name, givenType, body) => Some((name, givenType, body))
      case _ => None
    }
  }

  /** Frontend node to select a field from an object. Should be transformed to SelectField*/
  case class Select(obj: Value[SType], field: String, resType: Option[SType] = None) extends Value[SType] {
    override val opCode: OpCode = OpCodes.Undefined

    override def evaluated: Boolean = ???
    val tpe: SType = resType.getOrElse(obj.tpe match {
      case p: SProduct =>
        val i = p.methodIndex(field)
        if (i == -1) NoType
        else p.methods(i).stype
      case _ => NoType
    })

    def opType: SFunc = SFunc(obj.tpe, tpe)
  }

  case class Ident(name: String, tpe: SType = NoType) extends Value[SType] {
    override val opCode: OpCode = OpCodes.Undefined
    override def evaluated: Boolean = ???

    def opType: SFunc = SFunc(Vector(), tpe)
  }
  object Ident {
    def apply(name: String): Ident = Ident(name, NoType)
  }

  case class Apply(func: Value[SType], args: IndexedSeq[Value[SType]]) extends Value[SType] {
    override val opCode: OpCode = OpCodes.Undefined
    override def evaluated: Boolean = false
    lazy val tpe: SType = func.tpe match {
      case SFunc(_, r, _) => r
      case tCol: SCollectionType[_] => tCol.elemType
      case _ => NoType
    }

    def opType: SFunc = SFunc(Vector(func.tpe +: args.map(_.tpe):_*), tpe)
  }

  /** Apply types for type parameters of input value. */
  case class ApplyTypes(input: Value[SType], tpeArgs: Seq[SType]) extends Value[SType] { node =>
//    input.tpe.whenFunc(funcType =>
//      assert(funcType.tpeArgs.length == tpeArgs.length,
//        s"Invalid number of tpeArgs in $node: expected ${funcType.tpeArgs} but found $tpeArgs")
//    )
    override val opCode: OpCode = OpCodes.Undefined
    override def evaluated: Boolean = false
    lazy val tpe: SType = input.tpe match {
      case funcType: SFunc =>
        require(funcType.tpeParams.length == tpeArgs.length, s"Invalid number of type parameters in $node")
        val subst = funcType.tpeParams.map(_.ident).zip(tpeArgs).toMap
        SigmaTyper.applySubst(input.tpe, subst)
      case _ => input.tpe
    }
    /** This is not used as operation, but rather to form a program structure */
    def opType: SFunc = ???
  }

  case class MethodCall(obj: Value[SType], name: String, args: IndexedSeq[Value[SType]], tpe: SType = NoType) extends Value[SType] {

    override val opCode: OpCode = OpCodes.Undefined
    override def evaluated: Boolean = false

    def opType: SFunc = SFunc(obj.tpe +: args.map(_.tpe), tpe)
  }

  case class STypeParam(ident: STypeIdent, upperBound: Option[SType] = None, lowerBound: Option[SType] = None) {
    assert(upperBound.isEmpty && lowerBound.isEmpty, s"Type parameters with bounds are not supported, but found $this")
    override def toString = ident.toString + upperBound.fold("")(u => s" <: $u") + lowerBound.fold("")(l => s" >: $l")
  }

  case class Lambda(
        tpeParams: Seq[STypeParam],
        args: IndexedSeq[(String,SType)],
        givenResType: SType,
        body: Option[Value[SType]]) extends Value[SFunc]
  {
    require(!(tpeParams.nonEmpty && body.nonEmpty), s"Generic function definitions are not supported, but found $this")
    override val opCode: OpCode = OpCodes.Undefined
    override def evaluated: Boolean = false
    lazy val tpe: SFunc = SFunc(args.map(_._2), givenResType ?: body.fold(NoType: SType)(_.tpe), tpeParams)
    /** This is not used as operation, but rather to form a program structure */
    def opType: SFunc = SFunc(Vector(), tpe)
  }
  object Lambda {
    def apply(args: IndexedSeq[(String,SType)], resTpe: SType, body: Value[SType]): Lambda =
      Lambda(Nil, args, resTpe, Some(body))
    def apply(args: IndexedSeq[(String,SType)], body: Value[SType]): Lambda = Lambda(Nil, args, NoType, Some(body))
  }

  case class OperationId(name: String, opType: SFunc)

  implicit class ValueOps(val v: Value[SType]) extends AnyVal {
    def asValue[T <: SType]: Value[T] = v.asInstanceOf[Value[T]]
    def asNumValue: Value[SNumericType] = v.asInstanceOf[Value[SNumericType]]
    def asStringValue: Value[SString.type] = v.asInstanceOf[Value[SString.type]]
    def asBoolValue: Value[SBoolean.type] = v.asInstanceOf[Value[SBoolean.type]]
    def asIntValue: Value[SInt.type] = v.asInstanceOf[Value[SInt.type]]
    def asLongValue: Value[SLong.type] = v.asInstanceOf[Value[SLong.type]]
    def asSigmaBoolean: SigmaBoolean = v.asInstanceOf[SigmaBoolean]
    def asBox: Value[SBox.type] = v.asInstanceOf[Value[SBox.type]]
    def asGroupElement: Value[SGroupElement.type] = v.asInstanceOf[Value[SGroupElement.type]]
    def asSigmaProp: Value[SSigmaProp.type] = v.asInstanceOf[Value[SSigmaProp.type]]
    def asByteArray: Value[SByteArray] = v.asInstanceOf[Value[SByteArray]]
    def asBigInt: Value[SBigInt.type] = v.asInstanceOf[Value[SBigInt.type]]
    def asCollection[T <: SType]: Value[SCollection[T]] = v.asInstanceOf[Value[SCollection[T]]]
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
        mkUpcast(tV, targetType)
    }
  }
}
