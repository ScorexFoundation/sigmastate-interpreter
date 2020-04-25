package special.collections

import special.collection.{Coll, PairOfCols, CollOverArray, CReplColl}
import org.scalacheck.Gen
import org.scalatest.{PropSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class CollsTests extends PropSpec with PropertyChecks with Matchers with CollGens { testSuite =>
  import Gen._
  import special.collection.ExtensionMethods._

  property("Coll.indices") {
    val minSuccess = MinSuccessful(30)
    forAll(collGen, collGen, minSuccess) { (col1: Coll[Int], col2: Coll[Int]) =>
      col1.indices.toArray shouldBe col1.toArray.indices.toArray
    }
    forAll(superGen, minSuccess) { cl =>
      cl.indices.toArray shouldBe cl.toArray.indices.toArray
    }
  }

  // TODO col1.zip(col2).length shouldBe col1.arr.zip(col2.arr).length
  property("Coll.zip") {
  }

  property("Coll.flatMap") {
    forAll(containerOfN[Coll, Int](3, valGen), collGen) { (zs, col) =>
      val matrix = zs.map(_ => col)
      val res = zs.zip(matrix).flatMap(_._2)
      res.toArray shouldBe zs.toArray.flatMap(_ => col.toArray)
    }
  }

  property("Coll.segmentLength") {
    forAll(collGen, indexGen) { (col, from) =>
      col.segmentLength(lt0, from) shouldBe col.toArray.segmentLength(lt0, from)
    }

    val minSuccess = minSuccessful(30)
    forAll(superGen, indexGen, minSuccess) { (col, from) =>
      col.segmentLength(collMatchRepl, from) shouldBe col.toArray.segmentLength(collMatchRepl, from)
    }
  }

  property("Coll.indexWhere") {
    forAll(collGen, indexGen) { (col, from) =>
      col.indexWhere(eq0, from) shouldBe col.toArray.indexWhere(eq0, from)
      def p2(ab: (Int, Int)) = eq0(ab._1) && eq0(ab._2)
      col.zip(col).indexWhere(p2, from) shouldBe col.toArray.zip(col.toArray).indexWhere(p2, from)
    }
  }

  property("Coll.indexOf") {
    forAll(collGen, indexGen, valGen) { (col, from, elem) =>
      col.indexOf(elem, from) shouldBe col.toArray.indexOf(elem, from)
      col.zip(col).indexOf((elem, elem), from) shouldBe col.toArray.zip(col.toArray).indexOf((elem, elem), from)
    }
  }

  property("Coll.lastIndexWhere") {
    forAll(collGen, indexGen) { (col, end) =>
      col.lastIndexWhere(eq0, end) shouldBe col.lastIndexWhere(eq0, end)
      def p2(ab: (Int, Int)) = eq0(ab._1) && eq0(ab._2)
      col.zip(col).lastIndexWhere(p2, end) shouldBe col.toArray.zip(col.toArray).lastIndexWhere(p2, end)
    }
  }

  property("Coll.partition") {
    forAll(collGen) { col =>
      val (lsC, rsC) = col.partition(lt0)
      val (ls, rs) = col.toArray.partition(lt0)
      lsC.toArray shouldBe ls
      rsC.toArray shouldBe rs
    }
  }

  property("Coll.patch") {
    forAll(collGen, choose(-100, 100), collGen, replacedGen) { (col, from, patch, replaced) =>
      whenever(col.isValidIndex(from)) {
        val patchedC = col.patch(from, patch, replaced)
        val patched = col.toArray.patch(from, patch.toArray, replaced)
        patchedC.toArray shouldBe patched
      }
    }
  }

  property("Coll.updated") {
    forAll(collGen, indexGen, valGen, MinSuccessful(200)) { (col, index, elem) =>
      whenever(col.isValidIndex(index)) {
        val patchedC = col.updated(index, elem)
        val patched = col.toArray.updated(index, elem)
        patchedC.toArray shouldBe patched
      }
      whenever(col.isInstanceOf[CollOverArray[_]]) {
        an[IndexOutOfBoundsException] should be thrownBy {
          col.updated(col.length, elem)
        }
        an[IndexOutOfBoundsException] should be thrownBy {
          col.updated(-1, elem)
        }
      }
    }
  }

  property("Coll.updateMany") {
    forAll(collGen, indexesGen, MinSuccessful(200)) { (col, indexes) =>
      whenever(indexes.forall(col.isValidIndex(_))) {
        val updatedC = col.updateMany(indexes, indexes)
        val updated = col.toArray.clone()
        for (i <- indexes)
          updated.update(i, i)
        updatedC.toArray shouldBe updated
      }
      whenever(col.isInstanceOf[CollOverArray[_]]) {
        an[IndexOutOfBoundsException] should be thrownBy {
          col.updateMany(builder.fromItems(col.length), builder.fromItems(0))
        }
        an[IndexOutOfBoundsException] should be thrownBy {
          col.updateMany(builder.fromItems(-1), builder.fromItems(0))
        }
      }
    }
  }

  property("Coll methods") {
    forAll(collGen, indexGen) { (col, index) =>
      {
        val res = col.sum(monoid)
        res shouldBe col.toArray.sum
        val pairs = col.zip(col)
        val pairMonoid = builder.Monoids.pairMonoid(monoid, monoid)
        pairs.sum(pairMonoid) shouldBe ((res, res))
      }
      {
        val res = col.map(inc)
        res.toArray shouldBe col.toArray.map(inc)
        val pairs = col.zip(col)
        pairs.map(plusF).toArray shouldBe pairs.toArray.map(plusF)
      }
      {
        val res = col.filter(lt0)
        res.toArray shouldBe col.toArray.filter(lt0)
      }
      {
        val res = col.forall(lt0)
        val emptyRepl = builder.replicate(0, 10)
        val repl = builder.replicate(col.length, 10)
        res shouldBe col.toArray.forall(lt0)
        emptyRepl.forall(lt0) shouldBe Array[Int]().forall(lt0)
        col.zip(repl).forall(predF) shouldBe col.zip(repl).toArray.forall(predF)
      }
      {
        val res = col.exists(lt0)
        res shouldBe col.toArray.exists(lt0)
        builder.replicate(0, -10).exists(lt0) shouldBe Array[Int]().exists(lt0)
      }
      {
        val res = col.foldLeft[Int](0, plusF)
        res shouldBe col.toArray.foldLeft(0)(plus)
        val pairs = col.zip(col)
        val op = (in: (Int,(Int,Int))) => in._1 + in._2._1 + in._2._2
        pairs.foldLeft(0, op) shouldBe pairs.toArray.foldLeft(0)((b,a) => op((b,a)))
      }
      whenever(col.isValidIndex(index)) {
        val res = col(index)
        res shouldBe col.toArray(index)

        val res2 = col.getOrElse(index, index)
        res2 shouldBe col.toArray(index)
      }
      
      col.getOrElse(col.length, index) shouldBe index
      col.getOrElse(-1, index) shouldBe index
    }
    forAll(superGen, indexGen) { (col, index) =>
      whenever(col.isValidIndex(index)) {
        val res = col(index)
        res shouldBe col.toArray(index)
      }
    }
  }

  property("Coll.slice") {
    forAll(collGen, indexGen, indexGen) { (col, from, until) =>
      whenever(col.isValidIndex(until)) {
        val res = col.slice(from, until)
        res.toArray shouldBe col.toArray.slice(from, until)
      }
    }

    forAll(superGen, indexGen, indexGen) { (col, from, until) =>
      whenever(col.isValidIndex(until)) {
        val res = col.slice(from, until)
        res.toArray shouldBe col.toArray.slice(from, until)
      }
    }
  }

  property("Coll.append") {
    forAll(collGen, collGen, valGen, MinSuccessful(50)) { (col1, col2, v) =>

      {
        val res = col1.append(col2)
        res.toArray shouldBe (col1.toArray ++ col2.toArray)
        val pairs1 = col1.zip(col1)
        val pairs2 = col2.zip(col2)
        val apairs = pairs1.append(pairs2)
        apairs.toArray shouldBe (pairs1.toArray ++ pairs2.toArray)
      }

      {
        val repl1 = builder.replicate(col1.length, v)
        val repl2 = builder.replicate(col2.length, v)
        val arepl = repl1.append(repl2)
        assert(arepl.isInstanceOf[CReplColl[Int]])
        arepl.toArray shouldBe (repl1.toArray ++ repl2.toArray)
        
        val pairs1 = repl1.zip(repl1)
        val pairs2 = repl2.zip(repl2)
        val apairs = pairs1.append(pairs2)
        apairs.toArray shouldBe (pairs1.toArray ++ pairs2.toArray)

        apairs match {
          case ps: PairOfCols[_,_] =>
            assert(ps.ls.isInstanceOf[CReplColl[Int]])
            assert(ps.rs.isInstanceOf[CReplColl[Int]])
          case _ =>
            assert(false, "Invalid type")
        }
      }
    }
  }

  property("Coll.mapReduce") {
    import scalan.util.CollectionUtil.TraversableOps
    def m(x: Int) = (math.abs(x) % 10, x)
    forAll(collGen) { col =>
      val res = col.mapReduce(m, plusF)
      val (ks, vs) = builder.unzip(res)
      vs.toArray.sum shouldBe col.toArray.sum
      ks.length <= 10 shouldBe true
      res.toArray shouldBe col.toArray.toIterable.mapReduce(m)(plus).toArray
    }
  }

  property("Coll.groupBy") {
    def key(x: Int) = math.abs(x) % 10
    forAll(collGen) { col =>
      val res = col.groupBy(key)
      val (ks, vs) = builder.unzip(res)
      vs.flatten.toArray.sum shouldBe col.toArray.sum
      ks.length <= 10 shouldBe true
      val pairs = col.map(x => (key(x), x))
      val res2 = pairs.groupByKey
      val (ks2, vs2) = builder.unzip(res)
      ks shouldBe ks2
      vs shouldBe vs2
    }
  }

  property("Coll.reverse") {
    val minSuccess = minSuccessful(50)
    forAll(allGen, minSuccess) { col =>
      val res = col.reverse
      res.toArray shouldBe col.toArray.reverse
      val pairs = col.zip(col)
      pairs.reverse.toArray shouldBe pairs.toArray.reverse
// TODO should work
//      val c1 = col.asInstanceOf[Coll[Any]]
//      val appended = c1.append(c1)
//      appended.toArray shouldBe (c1.toArray ++ c1.toArray)
    }
  }

  property("Coll.take") {
    val minSuccess = minSuccessful(50)
    forAll(allGen, minSuccess) { col =>
      val n = col.length / 2
      val res = col.take(n)
      res.toArray shouldBe col.toArray.take(n)
      val pairs = col.zip(col)
      pairs.take(n).toArray shouldBe pairs.toArray.take(n)
    }
  }

  property("Coll.distinct") {
    forAll(collGen) { col =>
      val res = col.distinct
      res.toArray shouldBe col.toArray.distinct
      val pairs = col.zip(col)
      pairs.distinct.toArray shouldBe pairs.toArray.distinct
    }
    forAll(superGen) { col =>
        val res = col.distinct
        res.toArray shouldBe col.toArray.distinct
        val pairs = col.zip(col)
        pairs.distinct.toArray shouldBe pairs.toArray.distinct
    }
  }

  property("Coll.equals") {
    def checkColls(repl: Coll[_], coll: Coll[_]) = {
      assert(coll == repl)
      assert(repl == coll)
      repl.hashCode() shouldBe coll.hashCode()

      val zip1 = repl.zip(repl)
      val zip2 = repl.zip(coll)
      val zip3 = coll.zip(coll)
      val zip4 = coll.zip(repl)

      assert(zip1 == zip2)
      assert(zip2 == zip3)
      assert(zip3 == zip4)
      assert(zip4 == zip1)
      zip1.hashCode() shouldBe zip2.hashCode()
      zip2.hashCode() shouldBe zip3.hashCode()
      zip3.hashCode() shouldBe zip4.hashCode()
      zip4.hashCode() shouldBe zip1.hashCode()
    }
    val minSuccess = minSuccessful(50)
    forAll(byteGen, indexGen, minSuccess) { (x, n) =>
      val repl = builder.replicate(n, x)
      val coll = builder.fromArray(Array.fill(n)(x))

      checkColls(repl, coll)
    }
    forAll(shortGen, indexGen, minSuccess) { (x, n) =>
      val repl = builder.replicate(n, x)
      val coll = builder.fromArray(Array.fill(n)(x))

      checkColls(repl, coll)
    }
    forAll(intGen, indexGen, minSuccess) { (x, n) =>
      val repl = builder.replicate(n, x)
      val coll = builder.fromArray(Array.fill(n)(x))

      checkColls(repl, coll)
    }
    forAll(longGen, indexGen, minSuccess) { (x, n) =>
      val repl = builder.replicate(n, x)
      val coll = builder.fromArray(Array.fill(n)(x))

      checkColls(repl, coll)
    }
    forAll(charGen, indexGen, minSuccess) { (x, n) =>
      val repl = builder.replicate(n, x)
      val coll = builder.fromArray(Array.fill(n)(x))

      checkColls(repl, coll)
    }
    forAll(floatGen, indexGen, minSuccess) { (x, n) =>
      val repl = builder.replicate(n, x)
      val coll = builder.fromArray(Array.fill(n)(x))

      checkColls(repl, coll)
    }
    forAll (doubleGen, indexGen, minSuccess) { (x, n) =>
      val repl = builder.replicate(n, x)
      val coll = builder.fromArray(Array.fill(n)(x))

      checkColls(repl, coll)
    }
    forAll (indexGen, minSuccess) { (n) =>
      val replTrue = builder.replicate(n, true)
      val collTrue = builder.fromArray(Array.fill(n)(true))
      val replFalse = builder.replicate(n, false)
      val collFalse = builder.fromArray(Array.fill(n)(false))

      checkColls(replTrue, collTrue)
      checkColls(replFalse, collFalse)
    }
    forAll(indexGen, minSuccess) { n =>
      val repl = builder.replicate(n, builder.fromItems(Array(1, 2, 3)))
      val coll = builder.fromArray(Array.fill(n)(builder.fromItems(Array(1, 2, 3))))

      checkColls(repl, coll)
    }
    forAll(indexGen, indexGen, minSuccess) { (n, m) =>
      val repl = builder.replicate(n, builder.replicate(m, 1))
      val coll = builder.fromArray(Array.fill(n)(builder.fromArray(Array.fill(m)(1))))

      checkColls(repl, coll)
    }
    // This tuple tests fail with previous implementation
    forAll (byteGen, doubleGen, intGen, indexGen, minSuccess) { (b, d, i, n) =>
      val repl = builder.replicate(n, (b, i))
      val coll = builder.fromArray(Array.fill[(Byte, Int)](n)((b, i)))

      checkColls(repl, coll)
    }
    forAll (byteGen, doubleGen, intGen, indexGen, minSuccess) { (b, d, i, n) =>
      val repl = builder.replicate(n, (b, (i, (d, b))))
      val coll = builder.fromArray(Array.fill[(Byte, (Int, (Double, Byte)))](n)((b, (i, (d, b)))))

      checkColls(repl, coll)
    }
    forAll (byteGen, doubleGen, intGen, indexGen, indexGen, minSuccess) { (b, d, i, n, m) =>
      val repl = builder.replicate(n, (b, ((i, (("string", builder.replicate(m, n)), Array(1, 2, 3, 4))), (d, b))))
      val coll = builder.fromArray(Array.fill(n)((b, ((i, (("string", builder.fromArray(Array.fill(m)(n))), Array(1, 2, 3, 4))), (d, b)))))

      checkColls(repl, coll)
    }
  }


  property("PairColl.mapFirst") {
    val minSuccess = minSuccessful(30)

    forAll(collGen, minSuccess) { col =>
      val pairs = col.zip(col)
      pairs.mapFirst(inc).toArray shouldBe pairs.toArray.map { case (x, y) => (inc(x), y) }
      pairs.mapSecond(inc).toArray shouldBe pairs.toArray.map { case (x, y) => (x, inc(y)) }
    }
  }

  property("Coll.unionSet") {
    forAll(collGen, collGen) { (col1, col2) =>
      val res = col1.unionSet(col2)
      res.toArray shouldBe (col1.toArray.union(col2.toArray).distinct)
    }
    builder.replicate(2, 10).unionSet(builder.replicate(3, 10)).toArray shouldBe Array(10)
    forAll(superGen) {
      case cl1: Coll[(_, _)] => {
        val res = cl1.unionSet(cl1)
        res.toArray shouldBe (cl1.toArray.union(cl1.toArray).distinct)
      }
      case _ => assert(false, "Generator returned invalid PairColl")
    }
    /* TODO: simplify the above code
     * match-case removal gives the following compilation error:
        type mismatch;
        found   : special.collection.PairColl[_$1(in value res),_$2(in value res)] where type _$2(in value res), type _$1(in value res)
        required: special.collection.Coll[(_$1(in method getSuperGen), _$2(in method getSuperGen))]
          val res = col1.unionSet(col1)
     */
  }

  property("Coll.diff") {
    forAll(collGen, collGen) { (col1, col2) =>
      val res = col1.diff(col2)
      res.toArray shouldBe (col1.toArray.diff(col2.toArray))
    }
    forAll(superGen) {
      case col: Coll[(_, _)] =>
        val res = col.diff(col)
        res.toArray shouldBe (col.toArray.diff(col.toArray))
      case _ => assert(false, "Generator returned invalid PairColl") // TODO make similar gens
    }
    /* TODO: simplify the above code
     * match-case removal gives the following compilation error:
        type mismatch;
        found   : special.collection.PairColl[_$1(in value res),_$2(in value res)] where type _$2(in value res), type _$1(in value res)
        required: special.collection.Coll[(_$1(in method getSuperGen), _$2(in method getSuperGen))]
          val res = col.diff(col)
     */
    builder.replicate(2, 10).diff(builder.replicate(1, 10)).toArray shouldBe Array(10)
  }

  property("Coll.intersect") {
    forAll(collGen, collGen) { (col1, col2) =>
      val res = col1.intersect(col2)
      res.toArray shouldBe (col1.toArray.intersect(col2.toArray))
    }
    builder.replicate(2, 10).intersect(builder.replicate(3, 10)).toArray shouldBe Array(10, 10)
  }

  property("CollBuilder.xor") {
    forAll(bytesGen, bytesGen) { (col1, col2) =>
      val n = col1.length min col2.length
      val c1 = col1.take(n)
      val c2 = col2.take(n)
      builder.xor(c1, c2).toArray shouldBe c1.toArray.zip(c2.toArray).map { case (l,r) => (l ^ r).toByte }
    }
  }

  property("CollBuilder.outerJoin") {
    def test(col: Coll[Int]) = {
      val inner = col.indices
      val rightOnly = inner.map(i => i + col.length)
      val leftOnly = rightOnly.map(i => -i)

      val leftKeys = inner.append(leftOnly)
      val leftValues = col.append(col.map(x => x + 2))

      val rightKeys = inner.append(rightOnly)
      val rightValues = col.append(col.map(x => x + 3))

      val left  = builder.pairColl(leftKeys, leftValues)
      val right = builder.pairColl(rightKeys, rightValues)
      val res = builder.outerJoin(left, right)(l => l._2 - 2, r => r._2 - 3, i => i._2._1 + 5)
      val (ks, vs) = builder.unzip(res)
      vs.sum(monoid) shouldBe (col.sum(monoid) * 2 + col.map(_ + 5).sum(monoid))
    }
//    test(builder.fromItems(0))
//    val gen = containerOfN[Array, Int](100, choose(20, 100))
//        .map(xs => builder.fromArray(xs.distinct))
    forAll(collGen) { col =>
      test(col)
    }
  }

  property("CViewColl.correctWork") {
    forAll(collGen) { coll =>
      val view = builder.makeView(coll, complexFunction)
      val usual = coll.map(complexFunction)
      view.toArray shouldBe usual.toArray
    }
  }

  property("Coll equality") {
    val arr1 = Array[Int](1, 2, 3)
    val arr2 = Array[Int](1, 2, 3)
    val repl1 = Array[Int](1,1,1)
    val repl2 = Array[Int](1,1,1)
    val pairs1 = arr1.zip(repl1)
    val replPairs = repl1.zip(repl2)
    def ids = Array.tabulate(3) { i => Array.fill(32)(i.toByte) }
    def replIds = Array.fill(3) { Array.fill(32)(1) }
    val tokensArr = ids.zip(arr1)
    case class NoShrink[T](x: T)


    val collGen = Gen.oneOf(builder.fromArray(arr1), builder.fromArray(arr2), builder.fromItems(1, 2, 3)).map(NoShrink(_))
    val replGen = Gen.oneOf(builder.fromArray(repl1), builder.replicate(3, 1)).map(NoShrink(_))
    val idsGen = Gen.oneOf(builder.fromArray(ids), builder.fromArray(ids)).map(NoShrink(_))
    val replIdsGen = Gen.oneOf(builder.fromArray(replIds), builder.replicate(3, Array.fill(32)(1))).map(NoShrink(_))

    val pairsGen = Gen.oneOf(
      for { c1 <- collGen; c2 <- replGen } yield builder.pairColl(c1.x, c2.x): Coll[(Int, Int)],
      Gen.const(builder.fromArray(pairs1)),
      Gen.const(builder.fromItems((1, 1), (2, 1), (3, 1)))
    ).map(NoShrink(_))
    val replPairsGen = Gen.oneOf(
      for { c1 <- replGen; c2 <- replGen } yield builder.pairColl(c1.x, c2.x): Coll[(Int, Int)],
      Gen.const(builder.replicate(3, (1,1))),
      Gen.const(builder.fromArray(replPairs))
    ).map(NoShrink(_))

    val tokensGen = Gen.oneOf(
      for { c1 <- idsGen; c2 <- collGen } yield builder.pairColl(c1.x, c2.x): Coll[(Array[Byte], Int)],
      Gen.const(builder.fromArray(tokensArr)),
      Gen.const(builder.fromItems(tokensArr(0), tokensArr(1), tokensArr(2)))
    ).map(NoShrink(_))

    val minSuccess = MinSuccessful(30)
    
    forAll(collGen, collGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
    }
    forAll(replGen, replGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
    }
    forAll(collGen, replGen, minSuccess) { (c1, c2) =>
      assert(c1.x != c2.x)
      assert(c2.x != c1.x)
    }
    forAll(pairsGen, pairsGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
    }
    forAll(replPairsGen, replPairsGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
//      println(s"c1=$c1; c2=$c2")
    }

    forAll(idsGen, idsGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
//      println(s"c1=$c1; c2=$c2")
    }

    forAll(replIdsGen, replIdsGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
//      println(s"c1=$c1; c2=$c2")
    }

    forAll(tokensGen, tokensGen, minSuccess) { (c1, c2) =>
      assert(c1.x == c2.x)
      assert(c2.x == c1.x)
//      println(s"c1=$c1; c2=$c2")
    }

// TODO the following test fails because equality of Seq is not deep, and nested arrays are shallow compared
//    forAll(tokensGen, minSuccess) { c1 =>
//      println(s"c1=${c1.x.toArray.toSeq.map { case (id, v) => (id.toSeq, v) }}")
//      val tokens = c1.x
//      assert(tokens.toArray.toSeq == tokensArr.toSeq)
//    }
  }

}
