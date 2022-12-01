package sigmastate.eval

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform._
import scalan.RType
import scalan.RType._
import sigma.types.PrimViewType
import sigmastate.SType._
import sigmastate.Values.SigmaBoolean
import sigmastate._
import sigmastate.lang.exceptions.CostLimitException
import special.Types._
import spire.syntax.all.cfor

import java.math.BigInteger
import scala.reflect.ClassTag
import scala.util.Try

// TODO refactor: find better place for this methods after code cleanup and repo reorganization

/** Helper methods used as part of ErgoTree evaluation. */
object Evaluation {
  import special.collection._
  import special.sigma._

  def msgCostLimitError(cost: Long, limit: Long) = s"Estimated execution cost $cost exceeds the limit $limit"

  /** Helper method to accumulate cost while checking limit.
    * @param current current cost value
    * @param more    additional cost to add to the current value
    * @param limit   total cost limit
    * @return new increased cost when it doesn't exceed the limit
    * @throws CostLimitException
    */
  def addCostChecked(current: Long, more: Long, limit: Long): Long = {
    val newCost = java7.compat.Math.addExact(current, more)
    if (newCost > limit) {
      throw new CostLimitException(
        estimatedCost = newCost,
        message = msgCostLimitError(newCost, limit), cause = None)
    }
    newCost
  }

  /** Transforms a serializable ErgoTree type descriptor to the corresponding RType descriptor of SigmaDsl,
    * which is used during evaluation.
    */
  def stypeToRType[T <: SType](t: T): RType[T#WrappedType] = (t match {  // TODO optimize using memoization
    case SBoolean => BooleanType
    case SByte => ByteType
    case SShort => ShortType
    case SInt => IntType
    case SLong => LongType
    case SString => StringType
    case SAny => AnyType
    case SUnit => UnitType
    case SBigInt => BigIntRType
    case SBox => BoxRType
    case SContext => ContextRType
    case SGlobal => SigmaDslBuilderRType
    case SHeader => HeaderRType
    case SPreHeader => PreHeaderRType
    case SGroupElement => GroupElementRType
    case SAvlTree => AvlTreeRType
    case SSigmaProp => SigmaPropRType
    case tup: STuple if tup.items.length == 2 =>
      val tpeA = tup.items(0)
      val tpeB = tup.items(1)
      pairRType(stypeToRType(tpeA), stypeToRType(tpeB))
    case STuple(items) =>
      val types = items.toArray
      val len = types.length
      val rtypes = new Array[SomeType](len)
      cfor(0)(_ < len, _ + 1) { i =>
        rtypes(i) = stypeToRType(types(i)).asInstanceOf[SomeType]
      }
      tupleRType(rtypes)
    case c: SCollectionType[a] => collRType(stypeToRType(c.elemType))
    case o: SOption[a] => optionRType(stypeToRType(o.elemType))
    case SFunc(args, tpeRange, Nil) if args.length == 1 =>
      val tpeArg = args(0)
      funcRType(stypeToRType(tpeArg), stypeToRType(tpeRange))
    case _ =>
      sys.error(s"Don't know how to convert SType $t to RType")
  }).asInstanceOf[RType[T#WrappedType]]

  /** Transforms RType descriptor of SigmaDsl, which is used during evaluation,
    * to the corresponding serializable ErgoTree type descriptor,
    */
  def rtypeToSType[T](t: RType[T]): SType = t match { // TODO optimize using memoization
    case BooleanType => SBoolean
    case ByteType => SByte
    case ShortType => SShort
    case IntType => SInt
    case LongType => SLong
    case StringType => SString
    case AnyType => SAny
    case UnitType => SUnit

    case BigIntegerRType => SBigInt
    case BigIntRType => SBigInt

    case ECPointRType => SGroupElement
    case GroupElementRType => SGroupElement

    case AvlTreeRType => SAvlTree
    case ot: OptionType[_] => sigmastate.SOption(rtypeToSType(ot.tA))
    case BoxRType => SBox
    case ContextRType => SContext
    case SigmaDslBuilderRType => SGlobal
    case HeaderRType => SHeader
    case PreHeaderRType => SPreHeader
    case SigmaPropRType => SSigmaProp
    case SigmaBooleanRType => SSigmaProp
    case tup: TupleType => STuple(tup.items.map(t => rtypeToSType(t)))
    case at: ArrayType[_] => SCollection(rtypeToSType(at.tA))
    case ct: CollType[_] => SCollection(rtypeToSType(ct.tItem))
    case ft: FuncType[_,_] => SFunc(rtypeToSType(ft.tDom), rtypeToSType(ft.tRange))
    case pt: PairType[_,_] => STuple(rtypeToSType(pt.tFst), rtypeToSType(pt.tSnd))
    case pvt: PrimViewType[_,_] => rtypeToSType(pvt.tVal)
    case _ => sys.error(s"Don't know how to convert RType $t to SType")
  }

  /** Tries to reconstruct RType of the given value.
    * If not successfull returns failure. */
  def rtypeOf(value: Any): Try[RType[_]] = Try { value match {
    case arr if arr.getClass.isArray =>
      val itemClass = arr.getClass.getComponentType
      if (itemClass.isPrimitive) {
        val itemTag = ClassTag[Any](itemClass)
        RType.fromClassTag(itemTag)
      } else
        sys.error(s"Cannot compute rtypeOf($value): non-primitive type of array items")

    case coll: Coll[_] => collRType(coll.tItem)
    
    // all primitive types
    case _: Boolean => BooleanType
    case _: Byte  => ByteType
    case _: Short => ShortType
    case _: Int   => IntType
    case _: Long  => LongType
    case _: Char  => CharType
    case _: Float  => FloatType
    case _: Double  => DoubleType
    case _: String  => StringType
    case _: Unit  => UnitType

    case _: BigInteger => BigIntegerRType
    case _: special.sigma.BigInt => BigIntRType

    case _: ECPoint => ECPointRType
    case _: GroupElement => GroupElementRType

    case _: ErgoBox => ErgoBoxRType
    case _: Box => BoxRType

    case _: AvlTreeData => AvlTreeDataRType
    case _: AvlTree => AvlTreeRType

    case _: SigmaBoolean => SigmaBooleanRType
    case _: SigmaProp => SigmaPropRType
    case _: Context => ContextRType
    case _ =>
      sys.error(s"Don't know how to compute typeOf($value)")
  }}

  /** Convert SigmaDsl representation of tuple to ErgoTree serializable representation. */
  def fromDslTuple(value: Any, tupleTpe: STuple): Coll[Any] = value match {
    case t: Tuple2[_,_] => TupleColl(t._1, t._2)
    case a: Coll[Any]@unchecked if a.tItem == RType.AnyType => a
    case _ =>
      sys.error(s"Cannot execute fromDslTuple($value, $tupleTpe)")
  }

  /** Convert ErgoTree serializable representation of tuple to SigmaDsl representation. */
  def toDslTuple(value: Coll[Any], tupleTpe: STuple): Any = tupleTpe match {
    case t if t.items.length == 2 => (value(0), value(1))
    case _ => value
  }

}
