
import java.math.BigInteger
import scalan.RType
import scalan.RType.{GeneralType, SomeType}

import scala.reflect.classTag

/** The following implicit values are used as type descriptors of all the predefined Sigma types.
  * @see [[RType]] class
  */
package object sigma {
  /** Forces reflection data initialization */
  val coreLibReflection = CoreLibReflection

  implicit val BigIntRType: RType[BigInt] = GeneralType(classTag[BigInt])
  implicit val GroupElementRType: RType[GroupElement] = GeneralType(classTag[GroupElement])
  implicit val SigmaPropRType: RType[SigmaProp] = GeneralType(classTag[SigmaProp])
  implicit val AvlTreeRType:   RType[AvlTree]   = GeneralType(classTag[AvlTree])

  implicit val BoxRType:       RType[Box]       = GeneralType(classTag[Box])
  implicit val ContextRType:   RType[Context]   = GeneralType(classTag[Context])

  implicit val HeaderRType: RType[Header] = GeneralType(classTag[Header])
  implicit val PreHeaderRType: RType[PreHeader] = GeneralType(classTag[PreHeader])

  implicit val AnyValueRType: RType[AnyValue] = RType.fromClassTag(classTag[AnyValue])

  implicit val SigmaContractRType: RType[SigmaContract] = RType.fromClassTag(classTag[SigmaContract])
  implicit val SigmaDslBuilderRType: RType[SigmaDslBuilder] = RType.fromClassTag(classTag[SigmaDslBuilder])

  implicit val BigIntegerRType: RType[BigInteger] = GeneralType(classTag[BigInteger])

  /** Implicit resolution of `Coll[A]` type descriptor, given a descriptor of `A`. */
  implicit def collRType[A](implicit tA: RType[A]): RType[Coll[A]] = CollType[A](tA)

  implicit val collBuilderRType: RType[CollBuilder] = RType.fromClassTag(classTag[CollBuilder])

  private def sameLengthErrorMsg[A, B](xs: Coll[A], ys: Coll[B]) =
    s"Collections should have same length but was ${xs.length} and ${ys.length}:\n xs=$xs;\n ys=$ys"

  def requireSameLength[A, B](xs: Coll[A], ys: Coll[B]) = {
    require(xs.length == ys.length, sameLengthErrorMsg(xs, ys))
  }

  /** Generic representation of tuples values. */
  type TupleData = Coll[Any]

  /** Returns an RType object representing a tuple type with the given SomeType array types.
    *
    * @param types An array of SomeType representing the types of each item in the tuple.
    * @return An RType object for the tuple type.
    */
  def tupleRType(types: Array[SomeType]): RType[TupleData] = TupleType(types)
}