package scalan

import scala.reflect.ClassTag
import scala.annotation.implicitNotFound

/** Base type for all runtime type descriptors. */
@implicitNotFound(msg = "No Elem available for ${A}.")
abstract class RType[A] {
  /** Class tag suitable for construct instances of Array[A]. */
  def classTag: ClassTag[A]

  /** Syntactically correct type name (type expression as String) */
  def name: String = this.toString

  /** Returns true is data size of `x: A` is the same for all `x`.
    * This is useful optimizations of calculating sizes of collections. */
  def isConstantSize: Boolean
}

object RType {
  /** Helper to request RType instances from an implicit scope.*/
  def apply[A](implicit t: RType[A]): RType[A] = t

  /** Helper cast from untyped descriptor to the typed one. */
  @inline def asType[T](t: RType[_]): RType[T] = t.asInstanceOf[RType[T]]

  def fromClassTag[A](ctA: ClassTag[A]): RType[A] = (ctA match {
    case ClassTag.Boolean => BooleanType
    case ClassTag.Byte => ByteType
    case ClassTag.Short => ShortType
    case ClassTag.Int => IntType
    case ClassTag.Long => LongType
    case ClassTag.Char => CharType
    case ClassTag.Float => FloatType
    case ClassTag.Double => DoubleType
    case ClassTag.Unit => UnitType
    case _ => GeneralType[A](ctA)
  }).asInstanceOf[RType[A]]

  type SomeType = RType[_]

  /** Type descriptor for types with limited type information like Any, AnyRef, Nothing.
    * It also used to wrap class tags which are GenericClassTag instances
    * (also with limited information about type structure).
    * To describe structure of the type more precisely use concrete classes
    * in the hierarchy of RType.
    */
  case class GeneralType[A](classTag: ClassTag[A]) extends RType[A] {
    override def name: String = classTag match {
      case ClassTag.Any => "Any"
      case ClassTag.AnyRef => "AnyRef"
      case ClassTag.Nothing => "Nothing"
      case ct => ct.runtimeClass.getSimpleName()
    }
    override def isConstantSize: Boolean = false
  }

  /** Descriptor used to represent primitive types. */
  case class PrimitiveType[A](classTag: ClassTag[A]) extends RType[A] {
    override def name: String = classTag.toString()
    /** We assume all primitive types have inhabitants of the same size. */
    override def isConstantSize: Boolean = true
  }

  val AnyType      : RType[Any]      = GeneralType[Any]     (ClassTag.Any)
  val AnyRefType   : RType[AnyRef]   = GeneralType[AnyRef]  (ClassTag.AnyRef)
  val NothingType  : RType[Nothing]  = GeneralType[Nothing] (ClassTag.Nothing)

  implicit val BooleanType : RType[Boolean]  = PrimitiveType[Boolean] (ClassTag.Boolean)
  implicit val ByteType    : RType[Byte]     = PrimitiveType[Byte]    (ClassTag.Byte)
  implicit val ShortType   : RType[Short]    = PrimitiveType[Short]   (ClassTag.Short)
  implicit val IntType     : RType[Int]      = PrimitiveType[Int]     (ClassTag.Int)
  implicit val LongType    : RType[Long]     = PrimitiveType[Long]    (ClassTag.Long)
  implicit val CharType    : RType[Char]     = PrimitiveType[Char]    (ClassTag.Char)
  implicit val FloatType   : RType[Float]    = PrimitiveType[Float]   (ClassTag.Float)
  implicit val DoubleType  : RType[Double]   = PrimitiveType[Double]  (ClassTag.Double)
  implicit val UnitType    : RType[Unit]     = PrimitiveType[Unit]    (ClassTag.Unit)

  /** Descriptor of the type A narrowed to the single inhabitant `value`. */
  case class SingletonType[A](value: A, classTag: ClassTag[A])() extends RType[A] {
    override def isConstantSize: Boolean = true  // since the type has only one inhabitant
  }

  implicit case object StringType extends RType[String] {
    override def classTag: ClassTag[String] = ClassTag[String](classOf[String])
    override def name: String = "String"
    override def isConstantSize: Boolean = false
  }

  /** Descriptor of descriptor to represent type of RType[_] instances.
    * This describes any possible descriptor, disregarding its underlying type.
    * Thus the underlying type is assumed to be Any. */
  case object RTypeType extends RType[RType[_]] {
    val classTag: ClassTag[RType[_]] = ClassTag[RType[_]](classOf[RType[_]])
    override def name: String = s"RType[Any]"
    override def isConstantSize: Boolean = false  // since all type terms of different size
  }
  /** Implicitly obtain the single RTypeType descriptor casted to the given type A. */
  implicit def rtypeRType[A]: RType[RType[A]] = asType[RType[A]](RTypeType)

  implicit def pairRType[A,B](implicit tA: RType[A], tB: RType[B]): RType[(A,B)] = PairType(tA, tB)

  case class PairType[A,B](tFst: RType[A], tSnd: RType[B]) extends RType[(A,B)] {
    val classTag: ClassTag[(A, B)] = scala.reflect.classTag[(A,B)]
    override def name: String = s"(${tFst.name}, ${tSnd.name})"
    override def isConstantSize: Boolean = tFst.isConstantSize && tSnd.isConstantSize
  }
  implicit def extendPairType[A,B](pt: RType[(A,B)]): PairType[A,B] = pt.asInstanceOf[PairType[A,B]]

  implicit def eitherRType[A,B](implicit tA: RType[A], tB: RType[B]): RType[Either[A,B]] = EitherType(tA, tB)

  case class EitherType[A,B](tLeft: RType[A], tRight: RType[B]) extends RType[Either[A,B]] {
    val classTag: ClassTag[Either[A, B]] = scala.reflect.classTag[Either[A,B]]
    override def name: String = s"(${tLeft.name} | ${tRight.name})"
    override def isConstantSize: Boolean = tLeft.isConstantSize && tRight.isConstantSize
  }

  implicit def funcRType[A,B](implicit tDom: RType[A], tRange: RType[B]): RType[A => B] = FuncType(tDom, tRange)

  case class FuncType[A,B](tDom: RType[A], tRange: RType[B]) extends RType[A => B] {
    val classTag: ClassTag[A => B] = scala.reflect.classTag[A => B]
    override def name: String = s"${tDom.name} => ${tRange.name}"
    override def isConstantSize: Boolean = false
  }

  implicit def arrayRType[A](implicit tA: RType[A]): RType[Array[A]] = ArrayType(tA)

  case class ArrayType[A](tA: RType[A]) extends RType[Array[A]] {
    val classTag: ClassTag[Array[A]] = {
      implicit val ctA: ClassTag[A] = tA.classTag
      scala.reflect.classTag[Array[A]]
    }
    override def name: String = s"Array[${tA.name}]"
    override def isConstantSize: Boolean = false
  }

  implicit def optionRType[A](implicit tA: RType[A]): RType[Option[A]] = OptionType(tA)

  case class OptionType[A](tA: RType[A]) extends RType[Option[A]] {
    val classTag: ClassTag[Option[A]] = {
      implicit val ctA: ClassTag[A] = tA.classTag
      scala.reflect.classTag[Option[A]]
    }
    override def name: String = s"Option[${tA.name}]"
    override def isConstantSize: Boolean = tA.isConstantSize
  }

  /** Underlying type of Thunk[A] values (or by-name values of type A). */
  type ThunkData[A] = () => A

  implicit def thunkRType[A](implicit tA: RType[A]): RType[ThunkData[A]] = ThunkType(tA)

  case class ThunkType[A](tA: RType[A]) extends RType[ThunkData[A]] {
    val classTag: ClassTag[ThunkData[A]] = {
      implicit val ctA: ClassTag[A] = tA.classTag
      scala.reflect.classTag[ThunkData[A]]
    }
    override def name: String = s"Thunk[${tA.name}]"
    override def isConstantSize: Boolean = false
  }

}
