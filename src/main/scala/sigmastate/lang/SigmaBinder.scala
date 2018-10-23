package sigmastate.lang

import java.lang.reflect.InvocationTargetException
import java.math.BigInteger

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import sigmastate.lang.Terms._
import sigmastate._
import Values._
import org.ergoplatform._
import scorex.util.encode.Base58
import sigmastate.interpreter.CryptoConstants
import sigmastate.lang.exceptions.{BinderException, InvalidArguments, InvalidTypeArguments}
import sigmastate.serialization.ValueSerializer

class SigmaBinder(env: Map[String, Any], builder: SigmaBuilder) {
  import SigmaBinder._
  import SigmaPredef._
  import builder._

  /** Rewriting of AST with respect to environment to resolve all references to global names
    * and infer their types. */
  private def eval(e: SValue, env: Map[String, Any]): SValue = rewrite(reduce(strategy[SValue]({
    case Ident(n, NoType) => env.get(n) match {
      case Some(v) => v match {
        case arr: Array[Boolean] => Some(mkCollectionConstant[SBoolean.type](arr, SBoolean))
        case arr: Array[Byte] => Some(mkCollectionConstant[SByte.type](arr, SByte))
        case arr: Array[Short] => Some(mkCollectionConstant[SShort.type](arr, SShort))
        case arr: Array[Int] => Some(mkCollectionConstant[SInt.type](arr, SInt))
        case arr: Array[Long] => Some(mkCollectionConstant[SLong.type](arr, SLong))
        case v: Byte => Some(mkConstant[SByte.type](v, SByte))
        case v: Short => Some(mkConstant[SShort.type](v, SShort))
        case v: Int => Some(mkConstant[SInt.type](v, SInt))
        case v: Long => Some(mkConstant[SLong.type](v, SLong))
        case v: BigInteger => Some(mkConstant[SBigInt.type](v, SBigInt))
        case v: CryptoConstants.EcPointType => Some(mkConstant[SGroupElement.type](v, SGroupElement))
        case b: Boolean => Some(if(b) TrueLeaf else FalseLeaf)
        case b: ErgoBox => Some(mkConstant[SBox.type](b, SBox))
        case avl: AvlTreeData => Some(mkConstant[SAvlTree.type](avl, SAvlTree))
        case v: SValue => Some(v)
        case _ => None
      }
      case None => predefinedEnv.get(n) match {
        case Some(v) => Some(Ident(n, v.tpe))
        case None => n match {
          case "HEIGHT" => Some(Height)
          case "INPUTS" => Some(Inputs)
          case "OUTPUTS" => Some(Outputs)
          case "LastBlockUtxoRootHash" => Some(LastBlockUtxoRootHash)
          case "EmptyByteArray" => Some(ByteArrayConstant(Array.emptyByteArray))
          case "SELF" => Some(Self)
          case "None" => Some(mkNoneValue(NoType))
          case _ => None
        }
      }
    }

    // Rule: Array[Int](...) -->
    case e @ Apply(ApplyTypes(Ident("Array", _), Seq(tpe)), args) =>
      val resTpe = if (args.isEmpty) tpe
      else {
        val elemType = args(0).tpe
        if (elemType != tpe)
          error(s"Invalid construction of array $e: expected type $tpe, actual type $elemType")
        elemType
      }
      Some(mkConcreteCollection(args, resTpe))

    // Rule: Array(...) -->
    case Apply(Ident("Array", _), args) =>
      val tpe = if (args.isEmpty) NoType else args(0).tpe
      Some(mkConcreteCollection(args, tpe))

    // Rule: Some(x) -->
    case Apply(Ident("Some", _), args) =>
      val arg =
        if (args.length == 1) args(0)
        else error(s"Invalid arguments of Some: expected one argument but found $args")
      Some(mkSomeValue(arg))

    // Rule: min(x, y) -->
    case Apply(Ident("min", _), args) => args match {
      case Seq(l: SValue, r: SValue) =>
        Some(mkMin(l.asNumValue, r.asNumValue))
      case _ =>
        throw new InvalidArguments(s"Invalid arguments for min: $args")
    }

    // Rule: max(x, y) -->
    case Apply(Ident("max", _), args) => args match {
      case Seq(l: SValue, r: SValue) =>
        Some(mkMax(l.asNumValue, r.asNumValue))
      case _ =>
        throw new InvalidArguments(s"Invalid arguments for max: $args")
    }

    // Rule getVar[T](id) --> GetVar(id)
    case e @ Apply(ApplyTypes(GetVarSym, targs), args) =>
      if (targs.length != 1 || args.length != 1)
        error(s"Wrong number of arguments in $e: expected one type argument and one variable id")
      val id = args.head match {
        case LongConstant(i) => SByte.downcast(i)
        case IntConstant(i) => SByte.downcast(i)
        case ShortConstant(i) => SByte.downcast(i)
        case ByteConstant(i) => i
        case v => error(s"invalid type for var id, expected numeric, got $v")
      }
      Some(mkGetVar(id, targs.head))

    // Rule: lambda (...) = ... --> lambda (...): T = ...
    case lam @ Lambda(params, args, t, Some(body)) =>
      require(params.isEmpty)
      val b1 = eval(body, env)
      val newLam = mkLambda(args, t, Some(b1))
      if (newLam != lam) Some(newLam) else None

    // Rule: { e } --> e
    case Block(Seq(), body) => Some(body)

    case block @ Block(binds, t) =>
      val newBinds = for (Val(n, t, b) <- binds) yield {
        if (env.contains(n)) error(s"Variable $n already defined ($n = ${env(n)}")
        val b1 = eval(b, env)
        mkVal(n, if (t != NoType) t else b1.tpe, b1)
      }
      val t1 = eval(t, env)
      val newBlock = mkBlock(newBinds, t1)
      if (newBlock != block)
        Some(newBlock)
      else
        None
    case e @ Apply(ApplyTypes(DeserializeSym, targs), args) =>
      if (targs.length != 1)
        throw new InvalidTypeArguments(s"Wrong number of type arguments in $e: expected one type argument")
      if (args.length != 1)
        throw new InvalidArguments(s"Wrong number of arguments in $e: expected one argument")
      val str = args.head match {
        case StringConstant(s) => s
        case _ =>
          throw new InvalidArguments(s"invalid argument in $e: expected a string constant")
      }
      val bytes = Base58.decode(str).get
      Some(
        ValueSerializer.deserialize(bytes))
  })))(e)

  def bind(e: SValue): SValue =
    try eval(e, env)
    catch { case e: InvocationTargetException => throw e.getCause }
}

object SigmaBinder {
  def error(msg: String) = throw new BinderException(msg, None)
}
