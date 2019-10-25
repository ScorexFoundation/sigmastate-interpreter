package special.collection

import java.util
import java.util.Objects

import special.SpecialPredef

import scala.reflect.ClassTag
import scalan._
import scalan.util.CollectionUtil
import scalan.{Internal, NeverInline, Reified, RType}
import Helpers._
import debox.Buffer
import scalan.RType._
import spire.syntax.all._

import scala.runtime.RichInt

class CollOverArray[@specialized A](val toArray: Array[A])(implicit tA: RType[A]) extends Coll[A] {
  @Internal
  override def tItem: RType[A] = tA
  def builder: CollBuilder = new CollOverArrayBuilder
  @inline def length: Int = toArray.length
  @inline def apply(i: Int): A = toArray.apply(i)

  @NeverInline
  override def isEmpty: Boolean = length == 0

  @NeverInline
  override def nonEmpty: Boolean = length > 0

  @NeverInline
  override def isDefinedAt(idx: Int): Boolean = (idx >= 0) && (idx < length)

  @NeverInline
  def getOrElse(i: Int, default: A): A = if (i >= 0 && i < toArray.length) toArray(i) else default

  @NeverInline
  def map[@specialized B: RType](f: A => B): Coll[B] = {
    implicit val ctB = RType[B].classTag
    builder.fromArray(toArray.map(f))
  }

  def foreach(f: A => Unit): Unit = toArray.foreach(f)
  def exists(p: A => Boolean): Boolean = toArray.exists(p)
  def forall(p: A => Boolean): Boolean = toArray.forall(p)
  def filter(p: A => Boolean): Coll[A] = builder.fromArray(toArray.filter(p))

  @NeverInline
  def foldLeft[B](zero: B, op: ((B, A)) => B): B = toArray.foldLeft(zero)((b, a) => op((b, a)))

  def slice(from: Int, until: Int): Coll[A] = builder.fromArray(toArray.slice(from, until))
  def sum(m: Monoid[A]): A = toArray.foldLeft(m.zero)((b, a) => m.plus(b, a))
  @inline def zip[@specialized B](ys: Coll[B]): PairColl[A, B] = builder.pairColl(this, ys)

  @NeverInline
  def append(other: Coll[A]): Coll[A] = {
    if (toArray.length <= 0) return other
    val result = CollectionUtil.concatArrays(toArray, other.toArray)
    builder.fromArray(result)
  }

  @NeverInline
  def reverse: Coll[A] = {
    val limit = length
    val res = new Array[A](limit)
    cfor(0)(_ < limit, _ + 1) { i =>
      res(i) = toArray(limit - i - 1)
    }
    builder.fromArray(res)
  }

  @NeverInline
  def indices: Coll[Int] = builder.fromArray(toArray.indices.toArray)

  @NeverInline
  override def flatMap[B: RType](f: A => Coll[B]): Coll[B] = {
    implicit val ctB = RType[B].classTag
    builder.fromArray(toArray.flatMap(x => f(x).toArray))
  }

  @NeverInline
  override def segmentLength(p: A => Boolean, from: Int): Int = toArray.segmentLength(p, from)

  @NeverInline
  override def indexWhere(p: A => Boolean, from: Int): Int = toArray.indexWhere(p, from)

  @NeverInline
  override def lastIndexWhere(p: A => Boolean, end: Int): Int = toArray.lastIndexWhere(p, end)

  @NeverInline
  override def take(n: Int): Coll[A] = {
    if (n <= 0) builder.emptyColl
    else if (n >= length) this
    else {
      val res = Array.ofDim[A](n)
      Array.copy(toArray, 0, res, 0, n)
      builder.fromArray(res)
    }
  }

  @NeverInline
  override def partition(pred: A => Boolean): (Coll[A], Coll[A]) = {
    val (ls, rs) = toArray.partition(pred)
    (builder.fromArray(ls), builder.fromArray(rs))
  }

  @NeverInline
  override def patch(from: Int, patch: Coll[A], replaced: Int): Coll[A] = {
    // TODO optimize: avoid using `patch` as it do boxing
    val res = toArray.patch(from, patch.toArray, replaced).toArray
    builder.fromArray(res)
  }

  @NeverInline
  override def updated(index: Int, elem: A): Coll[A] = {
    // TODO optimize: avoid using `updated` as it do boxing
    val res = toArray.updated(index, elem)
    builder.fromArray(res)
  }

  @NeverInline
  override def updateMany(indexes: Coll[Int], values: Coll[A]): Coll[A] = {
    requireSameLength(indexes, values)
    val resArr = toArray.clone()
    var i = 0
    while (i < indexes.length) {
      val pos = indexes(i)
      if (pos < 0 || pos >= toArray.length) throw new IndexOutOfBoundsException(pos.toString)
      resArr(pos) = values(i)
      i += 1
    }
    builder.fromArray(resArr)
  }

  @NeverInline
  override def mapReduce[K: RType, V: RType](m: A => (K, V), r: ((V, V)) => V): Coll[(K, V)] = {
    val (keys, values) = Helpers.mapReduce(toArray, m, r)
    builder.pairCollFromArrays(keys, values)
  }

  @NeverInline
  override def unionSet(that: Coll[A]): Coll[A] = {
    val set = debox.Set.ofSize[A](this.length)
    val res = Buffer.ofSize[A](this.length)
    @inline def addItemToSet(x: A) = {
      if (!set(x)) {
        set.add(x)
        res += x
      }
    }
    def addToSet(arr: Array[A]) = {
      val limit = arr.length
      cfor(0)(_ < limit, _ + 1) { i =>
        val x = arr(i)
        addItemToSet(x)
      }
    }
    addToSet(this.toArray)

    that match {
      case repl: ReplColl[A@unchecked] if repl.length > 0 => // optimization
        addItemToSet(repl.value)
      case _ =>
        addToSet(that.toArray)
    }
    builder.fromArray(res.toArray())
  }

  @Internal
  protected def isAllPrimValue(value: A): Boolean = {
    cfor(0)(_ < length, _ + 1) { i =>
      if (this(i) != value) return false
    }
    true
  }

  @Internal
  protected def isAllDeepEquals(value: Any): Boolean = {
    cfor(0)(_ < length, _ + 1) { i =>
      if (!Objects.deepEquals(this(i), value)) return false
    }
    true
  }

  @Internal
  def isReplArray(len: Int, value: A): Boolean = {
    length == len && {
      if (tItem.classTag.runtimeClass.isPrimitive) {
        isAllPrimValue(value)
      } else {
        isAllDeepEquals(value)
      }
    }
  }

  @Internal
  override def equals(obj: scala.Any): Boolean = obj match {
    case obj: CollOverArray[_] if obj.tItem == this.tItem =>
      util.Objects.deepEquals(obj.toArray, toArray)
    case repl: CReplColl[A]@unchecked if repl.tItem == this.tItem =>
      isReplArray(repl.length, repl.value)
    case _ => false
  }

  @Internal
  override def hashCode() = CollectionUtil.deepHashCode(toArray)
}

class CollOverArrayBuilder extends CollBuilder {
  override def Monoids: MonoidBuilder = new MonoidBuilderInst

  @inline def pairColl[@specialized A, @specialized B](as: Coll[A], bs: Coll[B]): PairColl[A, B] = new PairOfCols(as, bs)

  @Internal
  override def fromMap[K: RType, V: RType](m: Map[K, V]): Coll[(K, V)] = {
    val (ks, vs) = Helpers.mapToArrays(m)
    pairCollFromArrays(ks, vs)
  }

  @Internal
  private def fromBoxedPairs[A, B](seq: Seq[(A, B)])(implicit tA: RType[A], tB: RType[B]): PairColl[A,B] = {
    val len = seq.length
    val resA = Array.ofDim[A](len)(tA.classTag)
    val resB = Array.ofDim[B](len)(tB.classTag)
    cfor(0)(_ < len, _ + 1) { i =>
      val item = seq.apply(i).asInstanceOf[(A,B)]
      resA(i) = item._1
      resB(i) = item._2
    }
    pairCollFromArrays(resA, resB)(tA, tB)
  }

  @NeverInline
  @Reified("T")
  def fromItems[T](items: T*)(implicit cT: RType[T]): Coll[T] = cT match {
    case pt: PairType[a,b] =>
      val tA = pt.tFst
      val tB = pt.tSnd
      fromBoxedPairs(items)(tA, tB)
    case _ =>
      new CollOverArray(items.toArray(cT.classTag))
  }

  @NeverInline
  def fromArray[@specialized T: RType](arr: Array[T]): Coll[T] = RType[T] match {
    case pt: PairType[a,b] =>
      val tA = pt.tFst
      val tB = pt.tSnd
      fromBoxedPairs[a,b](arr.asInstanceOf[Array[(a,b)]])(tA, tB)
    case _ =>
      new CollOverArray(arr)
  }

  @NeverInline
  def replicate[@specialized T: RType](n: Int, v: T): Coll[T] = RType[T] match {
    case pt: PairType[a,b] =>
      val tA = pt.tFst
      val tB = pt.tSnd
      val tuple = v.asInstanceOf[(a, b)]
      new PairOfCols(replicate(n, tuple._1)(tA), replicate(n, tuple._2)(tB))
    case _ =>
      new CReplColl(v, n)
  }

  @NeverInline
  def makeView[@specialized A, @specialized B: RType](source: Coll[A], f: A => B): Coll[B] = new CViewColl(source, f)

  @NeverInline
  def makePartialView[@specialized A, @specialized B: RType](source: Coll[A], f: A => B, calculated: Array[Boolean], calculatedItems: Array[B]): Coll[B] = {
    new CViewColl(source, f).fromPartialCalculation(calculated, calculatedItems)
  }

  @NeverInline
  def unzip[@specialized A, @specialized B](xs: Coll[(A,B)]): (Coll[A], Coll[B]) = xs match {
    case pa: PairColl[_,_] => (pa.ls, pa.rs)
    case _ =>
      val limit = xs.length
      implicit val tA = xs.tItem.tFst
      implicit val tB = xs.tItem.tSnd
      val ls = Array.ofDim[A](limit)
      val rs = Array.ofDim[B](limit)
      cfor(0)(_ < limit, _ + 1) { i =>
        val p = xs(i)
        ls(i) = p._1
        rs(i) = p._2
      }
      (fromArray(ls), fromArray(rs))
  }

  @NeverInline
  def xor(left: Coll[Byte], right: Coll[Byte]): Coll[Byte] = left.zip(right).map { case (l, r) => (l ^ r).toByte }

  @NeverInline
  override def emptyColl[T](implicit cT: RType[T]): Coll[T] = cT match {
    case pt: PairType[a,b] =>
      val ls = emptyColl(pt.tFst)
      val rs = emptyColl(pt.tSnd)
      asColl[T](pairColl(ls, rs))
    case _ =>
      new CollOverArray[T](Array[T]())
  }

  @NeverInline
  override def outerJoin[K: RType, L, R, O: RType]
      (left: Coll[(K, L)], right: Coll[(K, R)])
      (l: ((K, L)) => O, r: ((K, R)) => O, inner: ((K, (L, R))) => O): Coll[(K, O)] = {
    val res = CollectionUtil.outerJoin[K,L,R,O](left.toMap, right.toMap)(
      (k,lv) => l((k,lv)),
      (k,rv) => r((k,rv)),
      (k, lv, rv) => inner((k, (lv, rv))))
    fromMap(res)
  }

  @NeverInline
  override def flattenColl[A: RType](coll: Coll[Coll[A]]): Coll[A] = {
    implicit val ctA = RType[A].classTag
    val res = coll.map(xs => xs.toArray).toArray.flatten
    fromArray(res)
  }
}

class PairOfCols[@specialized L, @specialized R](val ls: Coll[L], val rs: Coll[R]) extends PairColl[L,R] {
  @Internal
  override private[collection] def isReplArray(len: Int, value: (L, R)): Boolean = {
    ls.isReplArray(len, value._1) && rs.isReplArray(len, value._2)
  }

  @Internal
  override def equals(that: scala.Any) = (this eq that.asInstanceOf[AnyRef]) || (that match {
    case that: PairColl[_,_] if that.tItem == this.tItem => ls == that.ls && rs == that.rs
    case that: ReplColl[(L,R)]@unchecked if that.tItem == this.tItem =>
      ls.isReplArray(that.length, that.value._1) &&
      rs.isReplArray(that.length, that.value._2)
    case _ => false
  })
  @Internal
  override def hashCode() = ls.hashCode() * 41 + rs.hashCode()
  @Internal @inline
  implicit def tL = ls.tItem
  @Internal @inline
  implicit def tR = rs.tItem

  @Internal
  override lazy val tItem: RType[(L, R)] = {
    RType.pairRType(tL, tR)
  }

  override def builder: CollBuilder = new CollOverArrayBuilder
  override def toArray: Array[(L, R)] = ls.toArray.zip(rs.toArray)
  @inline override def length: Int = if (ls.length <= rs.length) ls.length else rs.length
  @inline override def apply(i: Int): (L, R) = (ls(i), rs(i))

  @NeverInline
  override def isEmpty: Boolean = length == 0

  @NeverInline
  override def nonEmpty: Boolean = length > 0

  @NeverInline
  override def isDefinedAt(idx: Int): Boolean = ls.isDefinedAt(idx) && rs.isDefinedAt(idx)

  @NeverInline
  override def getOrElse(i: Int, default: (L, R)): (L, R) =
    if (i >= 0 && i < this.length)
      this.apply(i)
    else {
      val d = default // force thunk
      (d._1, d._2)
    }

  @NeverInline
  override def map[@specialized V: RType](f: ((L, R)) => V): Coll[V] = {
    val limit = ls.length
    val res = new Array[V](limit)
    cfor(0)(_ < limit, _ + 1) { i =>
      res(i) = f((ls(i), rs(i)))
    }
    new CollOverArray(res)
  }

  @NeverInline
  override def exists(p: ((L, R)) => Boolean): Boolean = {
    val len = ls.length
    var i = 0
    while (i < len) {
      val found = p((ls(i), rs(i)))
      if (found) return true
      i += 1
    }
    false
  }

  @NeverInline
  override def forall(p: ((L, R)) => Boolean): Boolean = {
    val len = ls.length
    var i = 0
    while (i < len) {
      val ok = p((ls(i), rs(i)))
      if (!ok) return false
      i += 1
    }
    true
  }
  @NeverInline
  override def filter(p: ((L, R)) => Boolean): Coll[(L,R)] = {
    val len = ls.length
    val resL: Buffer[L] = Buffer.empty[L](ls.tItem.classTag)
    val resR: Buffer[R] = Buffer.empty[R](rs.tItem.classTag)
    var i = 0
    while (i < len) {
      val l = ls.apply(i)
      val r = rs.apply(i)
      val ok = p((l, r))
      if (ok) {
        resL += l
        resR += r
      }
      i += 1
    }
    builder.pairCollFromArrays(resL.toArray(), resR.toArray())
  }

  @NeverInline
  override def foldLeft[B](zero: B, op: ((B, (L, R))) => B): B = {
    val limit = length
    var state = zero
    cfor(0)(_ < limit, _ + 1) { i =>
      val l = ls.apply(i)
      val r = rs.apply(i)
      state = op((state, (l, r)))
    }
    state
  }

  override def slice(from: Int, until: Int): PairColl[L,R] = builder.pairColl(ls.slice(from, until), rs.slice(from, until))

  def append(other: Coll[(L, R)]): Coll[(L,R)] = {
    val arrs = builder.unzip(other)
    builder.pairColl(ls.append(arrs._1), rs.append(arrs._2))
  }

  override def reverse: Coll[(L, R)] = {
    val lLen = ls.length
    val rLen = rs.length
    if (lLen == rLen) {
      builder.pairColl(ls.reverse, rs.reverse)
    } else if (lLen < rLen) {
      builder.pairColl(ls.reverse, rs.slice(0, lLen).reverse)
    } else {
      builder.pairColl(ls.slice(0, rLen).reverse, rs.reverse)
    }
  }

  @NeverInline
  override def sum(m: Monoid[(L, R)]): (L, R) = {
    val limit = length
    var state = m.zero
    cfor(0)(_ < limit, _ + 1) { i =>
      val l = ls.apply(i)
      val r = rs.apply(i)
      state = m.plus(state, (l, r))
    }
    state
  }

  def zip[@specialized B](ys: Coll[B]): PairColl[(L,R), B] = builder.pairColl(this, ys)

  override def indices: Coll[Int] = if (ls.length <= rs.length) ls.indices else rs.indices

  @NeverInline
  override def flatMap[B: RType](f: ((L, R)) => Coll[B]): Coll[B] =
    builder.fromArray(toArray.flatMap(p => f(p).toArray))

  @NeverInline
  override def segmentLength(p: ((L, R)) => Boolean, from: Int): Int = {
    toArray.segmentLength(p, from)
  }

  @NeverInline
  override def indexWhere(p: ((L, R)) => Boolean, from: Int): Int = toArray.indexWhere(p, from)

  @NeverInline
  override def lastIndexWhere(p: ((L, R)) => Boolean, end: Int): Int = toArray.lastIndexWhere(p, end)

  @NeverInline
  override def take(n: Int): Coll[(L, R)] = builder.pairColl(ls.take(n), rs.take(n))

  @NeverInline
  override def partition(pred: ((L, R)) => Boolean): (Coll[(L, R)], Coll[(L, R)]) = {
    val (ls, rs) = toArray.partition(pred)
    (builder.fromArray(ls), builder.fromArray(rs))
  }

  @NeverInline
  override def patch(from: Int, patch: Coll[(L, R)], replaced: Int): Coll[(L, R)] = {
    val (lsPatch, rsPatch) = builder.unzip(patch)
    val lp = ls.patch(from, lsPatch, replaced)
    val rp = rs.patch(from, rsPatch, replaced)
    builder.pairColl(lp, rp)
  }

  @NeverInline
  override def updated(index: Int, elem: (L, R)): Coll[(L, R)] = {
    val lu = ls.updated(index, elem._1)
    val ru = rs.updated(index, elem._2)
    builder.pairColl(lu, ru)
  }

  @NeverInline
  override def updateMany(indexes: Coll[Int], values: Coll[(L, R)]): Coll[(L, R)] = {
    requireSameLength(indexes, values)
    val resL = ls.toArray.clone()
    val resR = rs.toArray.clone()
    var i = 0
    while (i < indexes.length) {
      val pos = indexes(i)
      if (pos < 0 || pos >= length) throw new IndexOutOfBoundsException(pos.toString)
      resL(pos) = values(i)._1
      resR(pos) = values(i)._2
      i += 1
    }
    builder.pairColl(builder.fromArray(resL), builder.fromArray(resR))
  }

  @NeverInline
  override def mapReduce[K: RType, V: RType](m: ((L, R)) => (K, V), r: ((V, V)) => V): Coll[(K, V)] = {
    // TODO optimize: don't reify arr
    val (keys, values) = Helpers.mapReduce(toArray, m, r)
    builder.pairCollFromArrays(keys, values)
  }

  @NeverInline
  override def unionSet(that: Coll[(L, R)]): Coll[(L, R)] = {
    val set = new util.HashSet[(L,R)](32)
    implicit val ctL = ls.tItem.classTag
    implicit val ctR = rs.tItem.classTag
    val resL = Buffer.empty[L]
    val resR = Buffer.empty[R]
    def addToSet(item: (L,R)) = {
      if (!set.contains(item)) {
        set.add(item)
        resL += item._1
        resR += item._2
      }
    }
    var i = 0
    val thisLen = math.min(ls.length, rs.length)
    while (i < thisLen) {
      addToSet((ls(i), rs(i)))
      i += 1
    }
    i = 0
    val thatLen = that.length
    while (i < thatLen) {
      addToSet(that(i))
      i += 1
    }
    builder.pairCollFromArrays(resL.toArray, resR.toArray)
  }

  @NeverInline
  override def mapFirst[T1: RType](f: L => T1): Coll[(T1, R)] = {
    builder.pairColl(ls.map(f), rs)
  }

  @NeverInline
  override def mapSecond[T1: RType](f: R => T1): Coll[(L, T1)] = {
    builder.pairColl(ls, rs.map(f))
  }
}

class CReplColl[@specialized A](val value: A, val length: Int)(implicit tA: RType[A]) extends ReplColl[A] {
  @Internal
  override def tItem: RType[A] = tA

  def builder: CollBuilder = new CollOverArrayBuilder

  @Internal
  lazy val _toArray: Array[A] = {
    implicit val cT: ClassTag[A] = tA.classTag
    val res = new Array[A](length)
    val v = value
    cfor(0)(_ < length, _ + 1) { i => res(i) = v }
    res
  }
  @NeverInline
  def toArray = _toArray

  @NeverInline
  @inline def apply(i: Int): A = if (i >= 0 && i < this.length) value else throw new IndexOutOfBoundsException(i.toString)

  @NeverInline
  override def isEmpty: Boolean = length == 0

  @NeverInline
  override def nonEmpty: Boolean = length > 0

  @NeverInline
  override def isDefinedAt(idx: Int): Boolean = (idx >= 0 && idx < this.length)

  @NeverInline
  def getOrElse(i: Int, default: A): A = if (i >= 0 && i < this.length) value else default
  def map[@specialized B: RType](f: A => B): Coll[B] = new CReplColl(f(value), length)
  @NeverInline
  def foreach(f: A => Unit): Unit = (0 until length).foreach(_ => f(value))
  @NeverInline
  def exists(p: A => Boolean): Boolean = if (length == 0) false else p(value)
  @NeverInline
  def forall(p: A => Boolean): Boolean = if (length == 0) true else p(value)
  @NeverInline
  def filter(p: A => Boolean): Coll[A] =
    if (length == 0) this
    else
    if (p(value)) this
    else new CReplColl(value, 0)

  @NeverInline
  def foldLeft[B](zero: B, op: ((B, A)) => B): B =
    SpecialPredef.loopUntil[(B, Int)]((zero,0),
      p => p._2 >= length,
      p => (op((p._1, value)), p._2 + 1)
    )._1

  def zip[@specialized B](ys: Coll[B]): PairColl[A, B] = builder.pairColl(this, ys)

  @NeverInline
  def slice(from: Int, until: Int): Coll[A] = {
    val lo = math.max(from, 0)
    val hi = math.min(math.max(until, 0), length)
    val size = math.max(hi - lo, 0)
    new CReplColl(value, size)
  }

  @NeverInline
  def append(other: Coll[A]): Coll[A] = other match {
    case repl: ReplColl[A@unchecked] if this.value == repl.value =>
      new CReplColl(value, this.length + repl.length)
    case _ =>
      builder.fromArray(toArray).append(builder.fromArray(other.toArray))
  }

  override def reverse: Coll[A] = this

  def sum(m: Monoid[A]): A = m.power(value, length)

  @NeverInline
  override def indices: Coll[Int] = builder.fromArray((0 until length).toArray)

  @NeverInline
  override def flatMap[B: RType](f: A => Coll[B]): Coll[B] = {
    val seg = f(value).toArray
    val xs = Range(0, length).flatMap(_ => seg).toArray
    builder.fromArray(xs)
  }

  @NeverInline
  override def segmentLength(p: A => Boolean, from: Int): Int = {
    if (from >= length) 0
    else
    if (p(value)) length - from
    else 0
  }

  @NeverInline
  override def indexWhere(p: A => Boolean, from: Int): Int = {
    if (from >= length) -1
    else
    if (p(value)) math.max(from, 0)
    else -1
  }

  @NeverInline
  override def lastIndexWhere(p: A => Boolean, end: Int): Int = {
    var i = math.min(end, length - 1)
    if (i < 0) i
    else if (p(value)) i
    else -1
  }

  @NeverInline
  override def take(n: Int): Coll[A] =
    if (n <= 0) builder.emptyColl
    else {
      val m = new RichInt(n).min(length)
      new CReplColl(value, m)
    }

  @NeverInline
  override def partition(pred: A => Boolean): (Coll[A], Coll[A]) = {
    if (pred(value)) (this, builder.emptyColl[A])
    else (builder.emptyColl, this)
  }

  @NeverInline
  override def patch(from: Int, patch: Coll[A], replaced: Int): Coll[A] = {
    builder.fromArray(toArray.patch(from, patch.toArray, replaced))
  }

  @NeverInline
  override def updated(index: Int, elem: A): Coll[A] = {
    if (elem == value) this
    else {
      // TODO optimize: avoid using `updated` as it do boxing
      val res = toArray.updated(index, elem)
      builder.fromArray(res)
    }
  }

  @NeverInline
  override def updateMany(indexes: Coll[Int], values: Coll[A]): Coll[A] = {
    requireSameLength(indexes, values)
    val resArr = toArray.clone()
    var i = 0
    while (i < indexes.length) {
      val pos = indexes(i)
      if (pos < 0 || pos >= length) throw new IndexOutOfBoundsException(pos.toString)
      resArr(pos) = values(i)
      i += 1
    }
    builder.fromArray(resArr)
  }

  @NeverInline
  override def mapReduce[K: RType, V: RType](m: A => (K, V), r: ((V, V)) => V): Coll[(K, V)] = {
    if (length <= 0) return builder.pairColl(builder.emptyColl[K], builder.emptyColl[V])
    val (k, v) = m(value)
    var reducedV = v
    var i = 1
    while (i < length) {
      reducedV = r((reducedV, v))
      i += 1
    }
    builder.pairColl(builder.fromItems(k), builder.fromItems(reducedV))
  }

  @NeverInline
  override def unionSet(that: Coll[A]): Coll[A] = that match {
    case repl: ReplColl[A@unchecked] =>
      if (this.length > 0) {
        if (repl.length > 0) {
          if (value == repl.value) {
            // both replications have the same element `value`, just return it in a singleton set
            new CReplColl(value, 1)
          }
          else {
            builder.fromItems(value, repl.value)
          }
        }
        else
          new CReplColl(value, 1)
      } else {
        if (repl.length > 0) {
          new CReplColl(repl.value, 1)
        } else
          new CReplColl(value, 0)  // empty set
      }
    case _ =>
      if (this.length > 0)
        builder.fromItems(value).unionSet(that)
      else
        builder.emptyColl[A].unionSet(that)
  }

  @Internal
  override private[collection] def isReplArray(len: Int, value: A): Boolean = {
    this.length == len && this.value == value
  }

  @Internal
  override def equals(obj: scala.Any): Boolean = obj != null && (obj match {
    case repl: CReplColl[A]@unchecked if repl.tItem == this.tItem =>
      this.length == repl.length && this.value == repl.value
    case obj: Coll[A] if obj.tItem == this.tItem =>
      obj.isReplArray(this.length, this.value)
    case _ => false
  })

  @Internal
  override def hashCode() = CollectionUtil.deepHashCode(toArray)

  @Internal
  override def toString = s"ReplColl($value, $length)"
}
