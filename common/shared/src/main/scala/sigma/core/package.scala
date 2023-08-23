package sigma


import scala.reflect.ClassTag

package object core {

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
    * Note, this method allocates a new Buffer, but the underlying empty array is shared.
    * This is safe because empty arrays are immutable.
    */
  def emptyDBufferOfInt: debox.Buffer[Int] = debox.Buffer.unsafe(EmptyArrayOfInt)

}
