package special.collection

import scalan.{NeverInline, RType}
import spire.syntax.all.cfor


class CViewColl[@specialized A, @specialized B](val source: Coll[A], val f: A => B)(implicit val tItem: RType[B]) extends Coll[B] {

  private var isCalculated: Array[Boolean] = Array.ofDim[Boolean](source.length)(RType.BooleanType.classTag)
  private var items: Array[B] = Array.ofDim[B](source.length)(tItem.classTag)
  private var calculatedCount = 0

  def fromPartialCalculation(calculated: Array[Boolean], calculatedItems: Array[B]): CViewColl[A, B] = {
    if (calculated.length != source.length || calculatedItems.length != source.length)
      throw new RuntimeException("Can't make partial collection: calculated items dimension != source dimension")
    isCalculated = calculated
    items = calculatedItems
    calculatedCount = 0
    cfor(0)(_ < isCalculated.length, _ + 1) { i =>
      if (isCalculated(i)) {
        calculatedCount += 1
      }
    }

    this
  }

  private def isAllItemsCalculated(): Boolean = calculatedCount == length

  private def calculateItem(index: Int): Unit = {
    items(index) = f(source(index))
    isCalculated(index) = true
  }

  private def ensureItemNoCalcCountChange(index: Int): Unit = {
    if (!isCalculated(index)) {
      calculateItem(index)
    }
  }

  private def ensureItem(index: Int): Unit = {
    if (!isCalculated(index)) {
      calculateItem(index)
      calculatedCount += 1
    }
  }

  @inline private def ensureAndGetItem(index: Int): B = {
    ensureItem(index)
    items(index)
  }

  override def builder: CollBuilder = new CollOverArrayBuilder

  @NeverInline
  override def toArray: Array[B] = {
    if (!isAllItemsCalculated()) {
      cfor(0)(_ < length, _ + 1) { i =>
        ensureItemNoCalcCountChange(i)
      }
      calculatedCount = length
    }
    items
  }

  @NeverInline
  override def length: Int = source.length

  @NeverInline
  override def isEmpty: Boolean = source.isEmpty

  @NeverInline
  override def nonEmpty: Boolean = !isEmpty

  @NeverInline
  override def apply(i: Int): B = {
    if (!isDefinedAt(i))
      throw new ArrayIndexOutOfBoundsException()

    ensureAndGetItem(i)
  }

  @NeverInline
  override def isDefinedAt(idx: Int): Boolean = (idx >= 0) && (idx < length)

  @NeverInline
  override def getOrElse(index: Int, default: B): B = if (isDefinedAt(index)) ensureAndGetItem(index) else default

  @NeverInline
  override def map[@specialized C: RType](g: B => C): Coll[C] = builder.makeView(this, g)

  @NeverInline
  override def zip[@specialized C](ys: Coll[C]): Coll[(B, C)] = builder.pairColl(this, ys)

  @NeverInline
  override def exists(p: B => Boolean): Boolean = {
    cfor(0)(_ < length, _ + 1) { i =>
      val found = p(ensureAndGetItem(i))
      if (found) return true
    }
    false
  }

  @NeverInline
  override def forall(p: B => Boolean): Boolean = toArray.forall(p)

  @NeverInline
  override def filter(p: B => Boolean): Coll[B] = builder.fromArray(toArray)(tItem).filter(p)

  @NeverInline
  override def foldLeft[C](zero: C, op: ((C, B)) => C): C = toArray.foldLeft(zero)((item1, item2) => op((item1, item2)))

  @NeverInline
  override def indices: Coll[Int] = builder.fromArray((0 until source.length).toArray)

  @NeverInline
  override def flatMap[C: RType](g: B => Coll[C]): Coll[C] = builder.fromArray(toArray)(tItem).flatMap(g)

  @NeverInline
  override def segmentLength(p: B => Boolean, from: Int): Int = {
    val trueFrom = math.max(0, from)
    cfor(trueFrom)(_ < length, _ + 1) { i =>
      val checkResult = p(ensureAndGetItem(i))
      if (!checkResult) {
        return i - trueFrom
      }
    }
    length - trueFrom
  }

  @NeverInline
  override def indexWhere(p: B => Boolean, from: Int): Int = {
    val trueFrom = math.max(0, from)
    cfor(trueFrom)(_ < length, _ + 1) { i =>
      val found = p(ensureAndGetItem(i))
      if (found) return i
    }
    -1
  }

  @NeverInline
  override def lastIndexWhere(p: B => Boolean, end: Int): Int = toArray.lastIndexWhere(p, end)

  @NeverInline
  override def take(n: Int): Coll[B] = {
    if (n <= 0)
      return builder.emptyColl(tItem)
    if (n > length)
      return this
    slice(0, n)
  }

  @NeverInline
  override def partition(pred: B => Boolean): (Coll[B], Coll[B]) = builder.fromArray(toArray)(tItem).partition(pred)

  @NeverInline
  override def patch(from: Int,
                     patch: Coll[B],
                     replaced: Int): Coll[B] = {
    if (length > 0) {
      val start = math.max(0, from) // check if from is non-negative
      val trueReplace = math.max(replaced, 0) // check if replace is non-negative
      /*
       * According to patch specification new length is the old length + patch length - replacedCount.
       * replacedCount is min(trueReplace, length - start) since it can turn out that trueReplace is greater than
       * the rest length of array
       */
      val newLength = patch.length + length - math.min(trueReplace, length - start)

      // At first we copy all items at [0, start), since they are kept unchanged
      var itemsCopy = Array.ofDim[B](newLength)(tItem.classTag)
      Array.copy(items, 0, itemsCopy, 0, start)
      // There we put patch items after unchanged items from [0, start)
      Array.copy(patch.toArray, 0, itemsCopy, start, patch.length)
      // If there are any elements left in the rest of items and not all of them should be replaced, then we finally
      // copy them to the end of new items
      if (start + trueReplace < length)
        Array.copy(items, start + trueReplace, itemsCopy, start + patch.length, length - start - trueReplace)

      // Here's the same procedure as was with items
      var calcCopy = Array.ofDim[Boolean](newLength)(RType.BooleanType.classTag)
      Array.copy(isCalculated, 0, calcCopy, 0, start)
      if (start + trueReplace < length)
        Array.copy(isCalculated, start + trueReplace, calcCopy, start + patch.length, length - start - trueReplace)

      // mark patched items as calculated
      cfor(start)(_ < start + patch.length, _ + 1) { i =>
        calcCopy(i) = true
      }

      /*
       * patchColl solves problem with the absence of source elements in patch collection: it tries to copy the first
       * element as source for all elements of patch. If there's no any source elements (collection is empty) then
       * current collection is converted to CollOverArray and patch of CollOverArray is called (else branch below)
       */
      val patchColl = new CReplColl(source(0), patch.length)(source.tItem)
      builder.makePartialView(source.patch(start, patchColl, replaced), f, calcCopy, itemsCopy)(tItem)
    } else {
      builder.fromArray(toArray).patch(from, patch, replaced)
    }
  }

  @NeverInline
  override def updated(index: Int, elem: B): Coll[B] = {
    if (!isDefinedAt(index))
      throw new IndexOutOfBoundsException()

    var itemsCopy = items.clone()
    var calcCopy = isCalculated.clone()

    calcCopy(index) = true
    itemsCopy(index) = elem
    builder.makePartialView(source, f, calcCopy, itemsCopy)
  }

  @NeverInline
  override def updateMany(indexes: Coll[Int],
                          values: Coll[B]): Coll[B] = {
    // here we copy items and information about which items have been calculated already
    var itemsCopy = items.clone()
    var calcCopy = isCalculated.clone()

    // here we update items with new ones from values at indexes from indexes collection
    cfor(0)(_ < indexes.length, _ + 1) { i =>
      itemsCopy(indexes(i)) = values(i)
      calcCopy(indexes(i)) = true // updated elements should be surely marked as calculated
    }
    builder.makePartialView(source, f, calcCopy, itemsCopy)(tItem)
  }

  @NeverInline
  override def mapReduce[K: RType, V: RType](m: B => (K, V),
                                             r: ((V, V)) => V): Coll[(K, V)] = builder.fromArray(toArray)(tItem).mapReduce(m, r)

  @NeverInline
  override def unionSet(that: Coll[B]): Coll[B] = builder.fromArray(toArray)(tItem).unionSet(that)

  @NeverInline
  override def sum(m: Monoid[B]): B = toArray.foldLeft(m.zero)((b, a) => m.plus(b, a))

  @NeverInline
  override def slice(from: Int, until: Int): Coll[B] = {
    if (until <= 0 || until - from <= 0)
      return builder.emptyColl(tItem)

    val start = math.max(0, from)
    val end = math.min(until, length)
    val sliceLength = end - start

    val itemsCopy = Array.ofDim[B](sliceLength)(tItem.classTag)
    Array.copy(items, start, itemsCopy, 0, sliceLength)

    val calcCopy = Array.ofDim[Boolean](sliceLength)(RType.BooleanType.classTag)
    Array.copy(isCalculated, start, calcCopy, 0, sliceLength)

    builder.makePartialView(source.slice(from, until), f, calcCopy, itemsCopy)
  }

  @NeverInline
  override def append(other: Coll[B]): Coll[B] = builder.fromArray(toArray)(tItem).append(other)

  @NeverInline
  override def reverse: Coll[B] = {
    builder.makePartialView(source.reverse, f, isCalculated.reverse, items.reverse)(tItem)
  }

  override private[collection] def isReplArray(len: Int, value: B) = ???
}
