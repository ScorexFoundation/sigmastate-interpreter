package scalan.util

import scalan.BaseTests

import scala.collection.{Seq, mutable}
import scala.reflect.ClassTag

class CollectionUtilTests extends BaseTests {
  import scalan.util.CollectionUtil._
  import java.lang.{Byte => JByte, Integer}
  test("concatArrays") {
    val xs = Array[Byte](1,2,3)
    val ys = Array[Byte](4,5,6)
    val zs = concatArrays(xs, ys)
    assertResult(Array[Byte](1, 2, 3, 4, 5, 6))(zs)

//    val jxs = Array[JByte](new JByte(1), new JByte(2), new JByte(3))
//    val jys = Array[JByte](new JByte(4), new JByte(5), new JByte(6))
//    val jzs = concatArrays(jxs, jys)
//    assertResult(Array[Byte](1, 2, 3, 4, 5, 6))(jzs)
  }

  def join(l: Map[Int,Int], r: Map[Int,Int]) =
    outerJoin(l, r)((_,l) => l, (_,r) => r, (k,l,r) => l + r)
  def joinSeqs(l: Seq[Int], r: Seq[Int]) =
    outerJoinSeqs(l, r)(l => l, r => r)((_,l) => l, (_,r) => r, (k,l,r) => l + r).map(_._2)
  def joinPairs(l: Seq[(String,Int)], r: Seq[(String,Int)]) =
    outerJoinSeqs(l, r)(l => l._1, r => r._1)((_,l) => l._2, (_,r) => r._2, (k,l,r) => l._2 + r._2)

  test("outerJoin maps") {
    val left = Map(1 -> 1, 2 -> 2, 3 -> 3)
    val right = Map(2 -> 2, 3 -> 3, 4 -> 4)

    assertResult(Map(1 -> 1, 2 -> 4, 3 -> 6, 4 -> 4))(join(left,right))
    assertResult(Map(1 -> 1, 2 -> 2, 3 -> 3))(join(left,Map()))
    assertResult(Map(2 -> 2, 3 -> 3, 4 -> 4))(join(Map(), right))
    assertResult(Map(2 -> 4, 3 -> 6, 4 -> 8))(join(right, right))
  }

  test("outerJoinSeqs") {
    val left = Seq(1, 2, 3)
    val right = Seq(2, 3, 4)

    assertResult(Seq(1, 4, 6, 4))(joinSeqs(left, right))
    assertResult(Seq(1, 2, 3))(joinSeqs(left,Seq()))
    assertResult(Seq(2, 3, 4))(joinSeqs(Seq(), right))
    assertResult(Seq(4, 6, 8))(joinSeqs(right, right))

    val inner = Seq("a" -> 1, "b" -> 2, "c" -> 3)
    val outer = Seq("b" -> 2, "c" -> 3, "d" -> 4)

    assertResult(Seq("a" -> 1, "b" -> 4, "c" -> 6, "d" -> 4))(joinPairs(inner, outer))
    assertResult(Seq("a" -> 1, "b" -> 2, "c" -> 3))(joinPairs(inner,Seq()))
    assertResult(Seq("b" -> 2, "c" -> 3, "d" -> 4))(joinPairs(Seq(), outer))
    assertResult(Seq("b" -> 4, "c" -> 6, "d" -> 8))(joinPairs(outer, outer))
  }

  test("filterMap") {
    val xs = List(1, 2, 3)
    xs.filterMap(x => if (x <= 2) Some(s"x = $x") else None) should be(List("x = 1", "x = 2"))
  }

  test("mapUnzip") {
    val xs = Seq(1, 2, 3)

    {
      val (ints, strings, plus1s) = xs.mapUnzip(x => (x, x.toString, x + 1))
      ints shouldBe Seq(1, 2, 3)
      strings shouldBe Seq("1", "2", "3")
      plus1s shouldBe Seq(2, 3, 4)
    }
    
    {
      val (ints, strings) = xs.mapUnzip(x => (x, x.toString))
      ints shouldBe Seq(1, 2, 3)
      strings shouldBe Seq("1", "2", "3")
    }
  }

  test("mapFirst") {
    val xs = List(1, 2, 3)
    xs.findMap(x => if (x > 2) Some(s"x = $x") else None) should be(Some("x = 3"))
    xs.findMap(x => if (x > 3) Some(x) else None) should be(None)
  }

  val items: Iterable[(Int, String)] = Array((1, "a"), (2, "b"), (1, "c"))

  test("distinctBy") {
    val res = items.distinctBy(_._1)
    assertResult(Array((1, "a"), (2, "b")))(res)
  }

  test("mapReduce") {
    val res = items.mapReduce(p => (p._1, p._2))((v1, v2) => v1 + v2)
    assertResult(List((1, "ac"), (2, "b")))(res)
  }

  test("mergeWith") {
    type V = (Int, String)
    def key(p: V) = p._1
    def merge(v1: V, v2: V) = (v1._1, v1._2 + v2._2)

    {
      val res = List().mergeWith(List(), key, merge)
      assertResult(List())(res)
    }
    {
      val res = List((1, "a"), (2, "b"), (1, "c")).mergeWith(List(), key, merge)
      assertResult(List((1, "ac"), (2, "b")))(res)
    }
    {
      val res = List().mergeWith(List((1, "a"), (2, "b"), (1, "c")), key, merge)
      assertResult(List((1, "ac"), (2, "b")))(res)
    }
    {
      val ys = List((2, "c"), (3, "d"))
      val res = List((1, "a"), (2, "b"), (1, "c")).mergeWith(ys, key, merge)
      assertResult(List((1, "ac"), (2, "bc"), (3, "d")))(res)
    }
  }

  test("zipWithExpandedBy") {
    assertResult(Array((2, 0), (2, 1)))(2.zipWithExpandedBy(x => List.range(0,x)))
    assertResult(Array((3, 0), (3, 1), (3, 2)))(3.zipWithExpandedBy(x => List.range(0,x)))
  }

  def treeStep(tree: Array[List[Int]]): Int => List[Int] = i => tree(i)

  test("traverseDepthFirst") {
    {
      val tree = Array(
        List(1, 2), // 0
        List(),     // 1
        List(3),    // 2
        List())     // 3
      assertResult(List(0, 1, 2, 3))(0.traverseDepthFirst(treeStep(tree)))
    }
    {
      /*
       0
         1
           3
             5
             6
         2
           4
      */
      val tree = Array(
        List(1, 2),  // 0
        List(3),     // 1
        List(4),     // 2
        List(5,6),   // 3
        List(),      // 4
        List(),      // 5
        List()       // 6
      )
      assertResult(List(0, 1, 3, 5, 6, 2, 4))(0.traverseDepthFirst(treeStep(tree)))
    }
  }

  test("partitionByType") {
    val xs: List[Any] = List(1, "a", "b", 2, 3, 1.0, 2.0)
    val (ints, others) = xs.partitionByType[Integer, Any]
    ints shouldBe(List(1,2,3))
    val (strs, doubles) = others.partitionByType[String, Double]
    strs shouldBe(List("a", "b"))
    doubles shouldBe(List(1.0, 2.0))
  }

  test("mapConserve") {
    class A(val x: Int)
    val x = new A(10)
    val opt = Option(x)
    opt.mapConserve(a => a) shouldBe theSameInstanceAs(opt)
    opt.mapConserve(a => new A(a.x)) should not be theSameInstanceAs(opt)
  }

  test("transformConserve") {
    class A(val x: Int)
    val x = new A(10)
    x.transformConserve(a => a) shouldBe theSameInstanceAs(x)
    x.transformConserve(a => new A(a.x)) should not be theSameInstanceAs(x)
  }

  test("sameElements2") {
    Seq(1, 2).sameElements2(List(1, 2)) shouldBe true
    new mutable.WrappedArray.ofInt(Array(1, 2)).sameElements2(Vector(1, 2)) shouldBe true
    Seq(new mutable.WrappedArray.ofInt(Array(1, 2)), 3).sameElements2(Array(Vector(1, 2), 3)) shouldBe true
    Seq(Array(1, 2), 3).sameElements2(Array(Vector(1, 2), 3)) shouldBe true
    Seq(Array(1, 2), Option(3)).sameElements2(Array(Vector(1, 2), List(3))) shouldBe false

    Seq(1, 2).sameElements2(List(1, 2, 3)) shouldBe false
    new mutable.WrappedArray.ofInt(Array(1, 2, 3)).sameElements2(Vector(1, 2)) shouldBe false
    Seq(new mutable.WrappedArray.ofInt(Array(1, 2, 3)), 3).sameElements2(Array(Vector(1, 2), 3)) shouldBe false

  }

  def unboxedArray[T:ClassTag](in: Seq[T]): Array[T] = {
    in.toArray[T]
  }

  test("unboxedArray") {
    // empty list
    unboxedArray(Seq[Any]()).isInstanceOf[Array[Any]] shouldBe true

    // primitive types
    unboxedArray(Seq[Byte](java.lang.Byte.valueOf(1.toByte))).isInstanceOf[Array[Byte]] shouldBe true
    Seq[Any](java.lang.Byte.valueOf(1.toByte)).toArray.isInstanceOf[Array[Byte]] shouldBe false
    Seq[Byte](1.toByte).toArray[Byte].isInstanceOf[Array[Byte]] shouldBe true

    unboxedArray(Seq[Short](java.lang.Short.valueOf(1.toShort))).isInstanceOf[Array[Short]] shouldBe true
    unboxedArray(Seq[Int](java.lang.Integer.valueOf(1))).isInstanceOf[Array[Int]] shouldBe true
    unboxedArray(Seq[Long](java.lang.Long.valueOf(1))).isInstanceOf[Array[Long]] shouldBe true
    unboxedArray(Seq[Double](java.lang.Double.valueOf(1.0))).isInstanceOf[Array[Double]] shouldBe true
    unboxedArray(Seq[Float](java.lang.Float.valueOf(1.0f))).isInstanceOf[Array[Float]] shouldBe true
    unboxedArray(Seq[Boolean](java.lang.Boolean.valueOf(true))).isInstanceOf[Array[Boolean]] shouldBe true
    unboxedArray(Seq[Char](java.lang.Character.valueOf('a'))).isInstanceOf[Array[Char]] shouldBe true
    unboxedArray(Seq[String]("str")).isInstanceOf[Array[String]] shouldBe true

    // non-primitive type
    unboxedArray(Seq[Any](Option.empty[Boolean])).isInstanceOf[Array[Any]] shouldBe true
    unboxedArray(Seq[Seq[Any]](Seq())).isInstanceOf[Array[Seq[Any]]] shouldBe true
  }
}
