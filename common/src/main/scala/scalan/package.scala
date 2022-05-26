import spire.macros.Syntax

import scala.language.experimental.macros
import scala.reflect.ClassTag

package object scalan {
  type sp          = scala.specialized

  def cfor[A](init:A)(test:A => Boolean, next:A => A)(body:A => Unit): Unit =
    macro Syntax.cforMacro[A]

  /** Allows implicit resolution to find appropriate instance of ClassTag in
    * the scope where RType is implicitly available. */
  implicit def rtypeToClassTag[A](implicit t: RType[A]): ClassTag[A] = t.classTag

  /** Immutable empty array of integers, should be used instead of allocating new empty arrays. */
  val EmptyArrayOfInt = Array.empty[Int]

  /** Immutable empty Seq[Int] backed by empty array.
    * You should prefer using it instead of `Seq[Int]()` or `Seq.empty[Int]`
    */
  val EmptySeqOfInt: Seq[Int] = EmptyArrayOfInt

  /** Create a new empty buffer around pre-allocated empty array.
    * This method is preferred, rather that creating empty debox.Buffer directly
    * because it allows to avoid allocation of the empty array.
    */
  def emptyDBufferOfInt: debox.Buffer[Int] = debox.Buffer.unsafe(EmptyArrayOfInt)

}
