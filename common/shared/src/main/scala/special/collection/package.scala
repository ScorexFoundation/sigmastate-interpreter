package special

import scalan.RType

import scala.language.implicitConversions
import scala.reflect.classTag

package object collection {
  /** Forces reflection data initialization */
  val reflection = CoreLibReflection

  /** Implicit resolution of `Coll[A]` type descriptor, given a descriptor of `A`. */
  implicit def collRType[A](implicit tA: RType[A]): RType[Coll[A]] = CollType[A](tA)

  /** Conversion to underlying descriptor class.
    * Allows syntax like
    *
    * ```val tColl: RType[Coll[A]] = ...; tColl.tItem```
    *
    * where `tItem` is a method of `CollType`, but is not defined on `RType`.
    */
  implicit def downcastCollType[A](ct: RType[Coll[A]]): CollType[A] = ct.asInstanceOf[CollType[A]]

  implicit val collBuilderRType: RType[CollBuilder] = RType.fromClassTag(classTag[CollBuilder])

  private def sameLengthErrorMsg[A, B](xs: Coll[A], ys: Coll[B]) =
    s"Collections should have same length but was ${xs.length} and ${ys.length}:\n xs=$xs;\n ys=$ys"

  def requireSameLength[A, B](xs: Coll[A], ys: Coll[B]) = {
    require(xs.length == ys.length, sameLengthErrorMsg(xs, ys))
  }
}
