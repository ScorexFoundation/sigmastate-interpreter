package special.collection

import scalan.RType

object ExtensionMethods {

  implicit class CollOps[A](val source: Coll[A]) extends AnyVal {

    /** Tests whether every element of this $coll relates to the
      *  corresponding element of another sequence by satisfying a test predicate.
      *
      *  @param   that  the other sequence
      *  @param   p     the test predicate, which relates elements from both sequences
      *  @tparam  B     the type of the elements of `that`
      *  @return  `true` if both sequences have the same length and
      *                  `p(x, y)` is `true` for all corresponding elements `x` of this $coll
      *                  and `y` of `that`, otherwise `false`.
      */
    def corresponds[B](that: Coll[B])(p: ((A, B)) => Boolean): Boolean =
      source.zip(that).forall(p)

//    /** Returns the length of the longest prefix whose elements all satisfy some predicate.
//      *
//      *  $mayNotTerminateInf
//      *
//      *  @param   p     the predicate used to test elements.
//      *  @return  the length of the longest prefix of this $coll
//      *           such that every element of the segment satisfies the predicate `p`.
//      */
//    def prefixLength(p: A => Boolean): Int = xs.segmentLength(p, 0)
//
//    /** Finds index of first occurrence of some value in this $coll after or at some start index.
//      *
//      *  @param   elem   the element value to search for.
//      *  @param   from   the start index
//      *  @return  the index `>= from` of the first element of this $coll that is equal (as determined by `==`)
//      *           to `elem`, or `-1`, if none exists.
//      */
//    def indexOf(elem: A, from: Int): Int = xs.indexWhere(elem == _, from)
//
//    /** Finds index of last occurrence of some value in this $coll before or at a given end index.
//      *
//      *  @param   elem   the element value to search for.
//      *  @param   end    the end index.
//      *  @return  the index `<= end` of the last element of this $coll that is equal (as determined by `==`)
//      *           to `elem`, or `-1`, if none exists.
//      */
//    def lastIndexOf(elem: A, end: Int): Int = xs.lastIndexWhere(elem == _, end)
//
//    /** Finds index of last element satisfying some predicate.
//      *
//      *  @param   p     the predicate used to test elements.
//      *  @return  the index of the last element of this $coll that satisfies the predicate `p`,
//      *           or `-1`, if none exists.
//      */
//    def lastIndexWhere(p: A => Boolean): Int = xs.lastIndexWhere(p, xs.length - 1)
//

    /** Builds a new $coll from this $coll without any duplicate elements.
      *
      *  @return  A new $coll which contains the first occurrence of every element of this $coll.
      */
    def distinct: Coll[A] = {
      implicit val tA = source.tItem
      source.unionSet(source.builder.emptyColl[A])
    }
//
//    /** Tests whether this $coll starts with the given sequence.
//      *
//      * @param  that    the sequence to test
//      * @return `true` if this collection has `that` as a prefix, `false` otherwise.
//      */
////    def startsWith[B](that: Coll[B]): Boolean = startsWith(that, 0)
//
//    /** Tests whether this $coll contains the given sequence at a given index.
//      *
//      * '''Note''': If the both the receiver object `this` and the argument
//      * `that` are infinite sequences this method may not terminate.
//      *
//      * @param  that    the sequence to test
//      * @param  offset  the index where the sequence is searched.
//      * @return `true` if the sequence `that` is contained in this $coll at
//      *         index `offset`, otherwise `false`.
//      */
////    def startsWith[B](that: Coll[B], offset: Int): Boolean =
//
//    /** Tests whether this $coll ends with the given collection.
//      *  @param  that    the collection to test
//      *  @return `true` if this $coll has `that` as a suffix, `false` otherwise.
//      */
////    def endsWith(that: Coll[A]): Boolean
//
//    /** A copy of this $coll with an element value appended until a given target length is reached.
//      *
//      *  @param   len   the target length
//      *  @param   elem  the padding value
//      *  @return a new collection consisting of all elements of this $coll followed by the minimal
//      *          number of occurrences of `elem` so that the resulting collection has a length of at least `len`.
//      */
//    def padTo(len: Int, elem: A): Coll[A] = {
//      if (len <= xs.length) xs
//      else
//        xs.append(xs.builder.replicate(len - xs.length, elem))
//    }

  }

  implicit class PairCollOps[A,B](val source: Coll[(A,B)]) extends AnyVal {
    import RType._
    @inline implicit def tA = source.tItem.tFst
    @inline implicit def tB = source.tItem.tSnd

    // TODO optimize
    def unionSetByKey(that: Coll[(A,B)]): Coll[(A,B)] = {
      source.unionSetByKey(that)
    }

    def reduceByKey(r: ((B,B)) => B): Coll[(A,B)] = {
      source.mapReduce(identity, r)
    }

    def sumByKey(implicit m: Monoid[B]): Coll[(A,B)] =
      reduceByKey(r => m.plus(r._1, r._2))

    def groupByKey: Coll[(A, Coll[B])] = source.groupByProjecting(_._1, _._2)

    @inline def mapFirst[A1: RType](f: A => A1): Coll[(A1, B)] = source.asInstanceOf[PairColl[A,B]].mapFirst(f)
    @inline def mapSecond[B1: RType](f: B => B1): Coll[(A, B1)] = source.asInstanceOf[PairColl[A,B]].mapSecond(f)
  }

  implicit class NestedCollOps[A](val source: Coll[Coll[A]]) extends AnyVal {
    @inline implicit def tA = source.tItem.tItem

    def flatten: Coll[A] = source.builder.flattenColl(source)
  }

  implicit class OptionOps[A](val source: Option[A]) extends AnyVal {
    /** Returns a singleton collection containing the $option's value
      * if it is nonempty, or the empty collection if the $option is empty.
      * @since  2.0
      */
    def toColl(implicit tA: RType[A]): Coll[A] =
      if (source.isEmpty) Builder.DefaultCollBuilder.emptyColl[A]
      else Builder.DefaultCollBuilder.fromItems(source.get)
  }
}
