package special.collection

import scala.reflect.ClassTag
import scalan._

import scala.collection.immutable

/** Indexed (zero-based) collection of elements of type `A`
  * @define Coll `Coll`
  * @define coll collection
  * @define colls collections
  * @tparam A the collection element type
  */
@ContainerType
@FunctorType
@scalan.Liftable
@WithMethodCallRecognizers
trait Coll[@specialized A] {
  def builder: CollBuilder
  @Internal
  def toArray: Array[A]

  /** The length of the collection. */
  def length: Int

  /** The size of the collection in elements. */
  def size: Int = this.length

  /** Tests whether the $coll is empty.
    *  @return    `true` if the $coll contains no elements, `false` otherwise.
    */
  def isEmpty: Boolean

  /** Tests whether the $coll is not empty.
    *  @return    `true` if the $coll contains at least one element, `false` otherwise.
    */
  def nonEmpty: Boolean

  /** Returns true if the index in the valid range.
    * @param  i  index of an element of this collection */
  @Internal
  final def isValidIndex(i: Int): Boolean = 0 <= i && i < this.length

  /** The element at given index.
    *  Indices start at `0`; `xs.apply(0)` is the first element of collection `xs`.
    *  Note the indexing syntax `xs(i)` is a shorthand for `xs.apply(i)`.
    *
    *  @param  i       the index
    *  @return         the element at the given index
    *  @throws         ArrayIndexOutOfBoundsException if `i < 0` or `length <= i`
    */
  def apply(i: Int): A

  /** Tests whether this $coll contains given index.
    *
    *  The implementations of methods `apply` and `isDefinedAt` turn a `Coll[A]` into
    *  a `PartialFunction[Int, A]`.
    *
    * @param    idx     the index to test
    * @return   `true` if this $coll contains an element at position `idx`, `false` otherwise.
    */
  def isDefinedAt(idx: Int): Boolean

  /** The elements at given indexes.
    *  Indices start at `0` so that `xs.apply(0)` is the first element of collection `xs`.
    *  Note the indexing syntax `xs(i)` is a shorthand for `xs.apply(i)`.
    *
    *  @param  indexes   the indexes of the elements to extract from this collection
    *  @return       the elements at the given indexes
    *  @throws       ArrayIndexOutOfBoundsException if `i < 0` or `length <= i`
    *                                               for any `i` in `indexes`
    */
//  def applyMany(indexes: Coll[Int]): Coll[A]

  /** The element of the collection or default value.
    * If an index is out of bounds (`i < 0 || i >= length`) then `default` value is returned.
    * @param   i   the index
    * @return      the element at the given index or default value if index is out or bounds
    * @since 2.0
    */
  def getOrElse(index: Int, default: A): A

  /** Builds a new collection by applying a function to all elements of this collection.
  *
  *  @param f      the function to apply to each element.
  *  @tparam B     the element type of the returned collection.
  *  @return       a new collection of type `Coll[B]` resulting from applying the given function
  *                `f` to each element of this collection and collecting the results.
  */
  def map[@specialized B: RType](f: A => B): Coll[B]

  /** For this collection (x0, ..., xN) and other collection (y0, ..., yM)
    * produces a collection ((x0, y0), ..., (xK, yK)) where K = min(N, M) */
  def zip[@specialized B](ys: Coll[B]): Coll[(A, B)]

  /** Tests whether a predicate holds for at least one element of this collection.
    *  @param   p     the predicate used to test elements.
    *  @return        `true` if the given predicate `p` is satisfied by at least one element of this collection, otherwise `false`
    */
  def exists(p: A => Boolean): Boolean

  /** Tests whether a predicate holds for all elements of this collection.
    *  @param   p   the predicate used to test elements.
    *  @return      `true` if this collection is empty or the given predicate `p`
    *               holds for all elements of this collection, otherwise `false`.
    */
  def forall(p: A => Boolean): Boolean

  /** Selects all elements of this collection which satisfy a predicate.
    *  @param p     the predicate used to test elements.
    *  @return      a new collection consisting of all elements of this collection that satisfy the given
    *               predicate `p`. The order of the elements is preserved.
    *  @since 2.0
    */
  def filter(p: A => Boolean): Coll[A]

  /** Applies a binary operator to a start value and all elements of this collection,
    *  going left to right.
    *
    *  @param   z    the start value.
    *  @param   op   the binary operator.
    *  @tparam  B    the result type of the binary operator.
    *  @return  the result of inserting `op` between consecutive elements of this collection,
    *           going left to right with the start value `z` on the left:
    *           {{{
    *             op(...op(z, x_1), x_2, ..., x_n)
    *           }}}
    *           where `x,,1,,, ..., x,,n,,` are the elements of this collection.
    *           Returns `z` if this collection is empty.
    */
  def foldLeft[B](zero: B, op: ((B, A)) => B): B

  /** Produces the range of all indices of this collection as a new collection
    * containing [0 .. length-1] values.
    * @since 2.0
    */
  def indices: Coll[Int]

  /**
    * Builds a new collection by applying a function to all elements of this $coll
    * and using the elements of the resulting collections.
    *
    * Function `f` is constrained to be of the form `x => x.someProperty`, otherwise
    * it is illegal.
    *
    * @param f the function to apply to each element.
    * @tparam B the element type of the returned collection.
    * @return a new collection of type `Coll[B]` resulting from applying the given collection-valued function
    *         `f` to each element of this $coll and concatenating the results.
    *  @since 2.0
    */
  def flatMap[B: RType](f: A => Coll[B]): Coll[B]

  /** Computes length of longest segment whose elements all satisfy some predicate.
    *
    *  @param   p     the predicate used to test elements.
    *  @param   from  the index where the search starts.
    *  @return  the length of the longest segment of this $coll starting from index `from`
    *           such that every element of the segment satisfies the predicate `p`.
    *  @since 2.0
    */
  def segmentLength(p: A => Boolean, from: Int): Int

  /** Finds the first element of the $coll satisfying a predicate, if any.
    *
    *  @param p       the predicate used to test elements.
    *  @return        an option value containing the first element in the $coll
    *                 that satisfies `p`, or `None` if none exists.
    *  @since 2.0
    */
  @NeverInline
  def find(p: A => Boolean): Option[A] = {
    val i = segmentLength(!p(_), 0)
    if (i < length) Some(this(i)) else None
  }

  /** Finds index of the first element satisfying some predicate after or at some start index.
    *
    *  @param   p     the predicate used to test elements.
    *  @param   from   the start index
    *  @return  the index `>= from` of the first element of this $coll that satisfies the predicate `p`,
    *           or `-1`, if none exists.
    *  @since 2.0
    */
  def indexWhere(p: A => Boolean, from: Int): Int

  /** Finds index of first occurrence of some value in this $coll after or at some start index.
    *
    *  @param   elem   the element value to search for.
    *  @param   from   the start index
    *  @return  the index `>= from` of the first element of this $coll that is equal (as determined by `==`)
    *           to `elem`, or `-1`, if none exists.
    *  @since 2.0
    */
  @NeverInline
  def indexOf(elem: A, from: Int): Int = this.indexWhere(x => elem == x, from)

  /** Finds index of last element satisfying some predicate before or at given end index.
    *
    *  @param   p     the predicate used to test elements.
    *  @return  the index `<= end` of the last element of this $coll that satisfies the predicate `p`,
    *           or `-1`, if none exists.
    *  @since 2.0
    */
  def lastIndexWhere(p: A => Boolean, end: Int): Int

  /** Selects first ''n'' elements.
    *  @param  n    the number of elements to take from this $coll.
    *  @return a $coll consisting only of the first `n` elements of this $coll,
    *          or else the whole $coll, if it has less than `n` elements.
    *          If `n` is negative, returns an empty $coll.
    */
  def take(n: Int): Coll[A]

  /** Partitions this $coll in two $colls according to a predicate.
    *
    *  @param pred the predicate on which to partition.
    *  @return     a pair of $colls: the first $coll consists of all elements that
    *              satisfy the predicate `p` and the second $coll consists of all elements
    *              that don't. The relative order of the elements in the resulting ${coll}s
    *              will BE preserved (this is different from Scala's version of this method).
    *  @since 2.0
    */
  def partition(pred: A => Boolean): (Coll[A], Coll[A])

  /** Produces a new $coll where a slice of elements in this $coll is replaced by another sequence.
    *
    *  @param  from     the index of the first replaced element
    *  @param  patch    the replacement sequence
    *  @param  replaced the number of elements to drop in the original $coll
    *  @return          a new $coll consisting of all elements of this $coll
    *                   except that `replaced` elements starting from `from` are replaced by `patch`.
    *  @since 2.0
    */
  def patch(from: Int, patch: Coll[A], replaced: Int): Coll[A]

  /** A copy of this $coll with one single replaced element.
    *  @param  index  the position of the replacement
    *  @param  elem   the replacing element
    *  @return a new $coll which is a copy of this $coll with the element at position `index` replaced by `elem`.
    *  @throws IndexOutOfBoundsException if `index` does not satisfy `0 <= index < length`.
    *  @since 2.0
    */
  def updated(index: Int, elem: A): Coll[A]

  /** Returns a copy of this collection where elements at `indexes` are replaced with `values`.
    * @since 2.0
    */
  def updateMany(indexes: Coll[Int], values: Coll[A]): Coll[A]

  /** Apply m for each element of this collection, group by key and reduce each group using r.
    * @returns one item for each group in a new collection of (K,V) pairs.
    * @since 2.0
    */
  def mapReduce[K: RType, V: RType](m: A => (K,V), r: ((V,V)) => V): Coll[(K,V)]

  /** Partitions this $coll into a map of ${coll}s according to some discriminator function.
    *
    *  @param key   the discriminator function.
    *  @tparam K    the type of keys returned by the discriminator function.
    *  @return      A map from keys to ${coll}s such that the following invariant holds:
    *               {{{
    *                 (xs groupBy key)(k) = xs filter (x => key(x) == k)
    *               }}}
    *               That is, every key `k` is bound to a $coll of those elements `x`
    *               for which `key(x)` equals `k`.
    *  @since 2.0
    */
  @NeverInline
  def groupBy[K: RType](key: A => K): Coll[(K, Coll[A])] = {
    val res = toArray.groupBy(key).mapValues(builder.fromArray(_))
    builder.fromMap(res)
  }

  /** Partitions this $coll into a map of ${coll}s according to some discriminator function.
    * Additionally projecting each element to a new value.
    *
    *  @param key   the discriminator function.
    *  @param proj  projection function to produce new value for each element of this $coll
    *  @tparam K    the type of keys returned by the discriminator function.
    *  @tparam V    the type of values returned by the projection function.
    *  @return      A map from keys to ${coll}s such that the following invariant holds:
    *               {{{
    *                 (xs groupByProjecting (key, proj))(k) = xs filter (x => key(x) == k).map(proj)
    *               }}}
    *               That is, every key `k` is bound to projections of those elements `x`
    *               for which `key(x)` equals `k`.
    *  @since 2.0
    */
  @NeverInline
  def groupByProjecting[K: RType, V: RType](key: A => K, proj: A => V): Coll[(K, Coll[V])] = {
    implicit val ctV: ClassTag[V] = RType[V].classTag
    val res = toArray.groupBy(key).mapValues(arr => builder.fromArray(arr.map(proj)))
    builder.fromMap(res)
  }

  /** Produces a new collection which contains all distinct elements of this $coll and also all elements of
    *  a given collection that are not in this collection.
    *  This is order preserving operation considering only first occurrences of each distinct elements.
    *  Any collection `xs` can be transformed to a sequence with distinct elements by using xs.unionSet(Col()).
    *
    *  NOTE: Use append if you don't need set semantics.
    *
    *  @param that   the collection to add.
    *  @since 2.0
    */
  def unionSet(that: Coll[A]): Coll[A]

  /** Computes the multiset difference between this $coll and another sequence.
    *
    *  @param that   the sequence of elements to remove
    *  @tparam B     the element type of the returned $coll.
    *  @return       a new collection which contains all elements of this $coll
    *                except some of occurrences of elements that also appear in `that`.
    *                If an element value `x` appears
    *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will not form
    *                part of the result, but any following occurrences will.
    *  @since 2.0
    */
  @NeverInline
  def diff(that: Coll[A]): Coll[A] = {
    val res = toArray.diff(that.toArray)
    builder.fromArray(res)
  }

  /** Computes the multiset intersection between this $coll and another sequence.
    *
    *  @param that   the sequence of elements to intersect with.
    *  @return       a new collection which contains all elements of this $coll
    *                which also appear in `that`.
    *                If an element value `x` appears
    *                ''n'' times in `that`, then the first ''n'' occurrences of `x` will be retained
    *                in the result, but any following occurrences will be omitted.
    *  @since 2.0
    */
  @NeverInline
  def intersect(that: Coll[A]): Coll[A] = {
    val res = toArray.intersect(that.toArray)
    builder.fromArray(res)
  }

  /** Folding through all elements of this $coll starting from m.zero and applying m.plus to accumulate
    * resulting value.
    *
    *  @param m monoid object to use for summation
    *  @return  result of the following operations (m.zero `m.plus` x1 `m.plus` x2 `m.plus` ... xN)
    *  @since 2.0
    */
  def sum(m: Monoid[A]): A

  /** Selects an interval of elements.  The returned collection is made up
    *  of all elements `x` which satisfy the invariant:
    *  {{{
    *    from <= indexOf(x) < until
    *  }}}
    *  @param from   the lowest index to include from this $coll.
    *  @param until  the lowest index to EXCLUDE from this $coll.
    */
  def slice(from: Int, until: Int): Coll[A]

  /** Puts the elements of other collection after the elements of this collection (concatenation of 2 collections) */
  def append(other: Coll[A]): Coll[A]

  /** Returns new $coll with elements in reversed order.
    *
    *  @return A new $coll with all elements of this $coll in reversed order.
    */
  def reverse: Coll[A]

  @Internal
  private[collection] def isReplArray(len: Int, value: A): Boolean

  @Internal
  private def trim[T](arr: Array[T]) = arr.take(arr.length min 100)
  @Internal
  override def toString = s"Coll(${trim(toArray).mkString(",")})"

  @Internal
  implicit def tItem: RType[A]

  @Internal
  def toMap[T, U](implicit ev: A <:< (T, U)): immutable.Map[T, U] = {
    var b = immutable.Map.empty[T,U]
    var i = 0
    val len = length
    while (i < len) {
      val kv = this(i)
      if (b.contains(kv._1))
        throw new IllegalArgumentException(s"Cannot transform collection $this to Map: duplicate key in entry $kv")
      b = b + kv
      i += 1
    }
    b
  }

  @Internal
  def distinctByKey[T, U](implicit ev: A <:< (T, U)): Coll[A] = {
    unionSetByKey(builder.emptyColl[A](tItem))
  }


  @Internal
  def unionSetByKey[T, U](that: Coll[A])(implicit ev: A <:< (T, U)): Coll[A] = {
    import scalan.util.CollectionUtil._
    // TODO optimize representation-wise
    val res = append(that).toArray.toIterable.distinctBy(_._1)
    builder.fromArray(res.toArray(tItem.classTag))
  }
}

@WithMethodCallRecognizers
trait PairColl[@specialized L, @specialized R] extends Coll[(L,R)] {
  def ls: Coll[L]
  def rs: Coll[R]
  def mapFirst[T1: RType](f: L => T1): Coll[(T1, R)]
  def mapSecond[T1: RType](f: R => T1): Coll[(L, T1)]
}

@Liftable
@WithMethodCallRecognizers
trait ReplColl[@specialized A] extends Coll[A] {
  def value: A
  def length: Int
  def append(other: Coll[A]): Coll[A]
}

@scalan.Liftable
@WithMethodCallRecognizers
trait CollBuilder {
  def Monoids: MonoidBuilder
  def pairColl[@specialized A, @specialized B](as: Coll[A], bs: Coll[B]): PairColl[A,B]

  @Internal
  def pairCollFromArrays[A: RType, B: RType](as: Array[A], bs: Array[B]): PairColl[A,B] =
    pairColl(fromArray(as), fromArray(bs))

  /** Construct a collection of (K,V) pairs using PairColl representation,
    * in which keys and values are stored as separate unboxed arrays. */
  @Internal
  def fromMap[K: RType, V: RType](m: Map[K,V]): Coll[(K,V)]

  /** Construct a new collection from the given list of arguments.
    * The arguments should be of the same type for which there should be
    * an implicit type descriptor at the call site. */
  @Reified("T") def fromItems[T](items: T*)(implicit cT: RType[T]): Coll[T]

  /** Deconstruct collection of (A,B) pairs into pair of collections.
    * If `xs` is represented as PairColl, then this is O(1) operation (no data is touched). */
  def unzip[@specialized A, @specialized B](xs: Coll[(A,B)]): (Coll[A], Coll[B])

  /** Element-wise xor of two collections. */
  def xor(left: Coll[Byte], right: Coll[Byte]): Coll[Byte]

  /** Wrap array into collection. */
  @Internal
  def fromArray[@specialized T: RType](arr: Array[T]): Coll[T]

  /** Creates a new collection by replicating value `v`.
    * @param  n  how many times to replicate value `v`
    * @param  v  value to replicate
    * @return    collection of the form (v, v, v, ... v) of n elements.*/
  def replicate[@specialized T: RType](n: Int, v: T): Coll[T]

  /** Create a new collection in which every item is executed lazily
    * form the corresponding item of the `source` collection.
    * @param  source  collection which is used as the source of items
    * @param  f       function to compute each item of this collection from the source item
    * This is O(1) operation, all executions of `f` are delayed until the corresponding
    * item of this collection is needed in some operation.
    */
  @Internal
  def makeView[@specialized A, @specialized B: RType](source: Coll[A], f: A => B): Coll[B]

  @Internal
  def makePartialView[@specialized A, @specialized B: RType](source: Coll[A], f: A => B, calculated: Array[Boolean], calculatedItems: Array[B]): Coll[B]

  /** Create an empty collection with items of the given type.
    * Even though there are no items, the type of them is specified. */
  def emptyColl[T](implicit tT: RType[T]): Coll[T]

  /** Performs outer join operation between left and right collections.
    * This is a restricted version of relational join operation.
    * It expects `left` and `right` collections have distinct K values in pairs (otherwise exception is thrown).
    * Under this condition resulting collection has size <= left.size + right.size.
    * @param l projection function executed for each element of `left`
    * @param r projection function executed for each element of `right`
    * @param inner projection function which is executed for matching items (K, L) and (K, R) with the same K
    * @return collection of (K, O) pairs, where each key comes form either left or right collection and values are produced by projections
    * @since 2.0
    */
  def outerJoin[K: RType, L, R, O: RType]
      (left: Coll[(K, L)], right: Coll[(K, R)])
      (l: ((K,L)) => O, r: ((K,R)) => O, inner: ((K,(L,R))) => O): Coll[(K,O)]

  /** Flattens a two-dimensional array by concatenating all its rows
    *  into a single array.
    *
    *  @tparam U        Type of row elements.
    *  @param asTrav    A function that converts elements of this array to rows - arrays of type `U`.
    *  @return          An array obtained by concatenating rows of this array.
    */
  def flattenColl[A:RType](coll: Coll[Coll[A]]): Coll[A]
}

