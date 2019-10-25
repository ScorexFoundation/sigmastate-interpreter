package special.collections

import scala.collection.mutable
import scala.language.reflectiveCalls
import scalan.util.BenchmarkUtil._
import special.SpecialPredef

class CostedTests extends BaseCostedTests {

  class ThisCtx extends Ctx  {
  }
  lazy val ctx = new ThisCtx
  import ctx._
  import CSizePrim._
  import CSizePair._
  import CSizeColl._
  import CSizeOption._
  import Costed._
  import CCostedPair._
  import CCostedPrim._
  import CCostedColl._
  import CCostedOption._
  import CollBuilder._
  import CostedBuilder._
  import Coll._
  import WOption._
  import WSpecialPredef._
  import Liftables._

  def buildGraph[T](nIters: Int, name: String)(action: Int => Ref[T]) = {
    val buf = mutable.ArrayBuilder.make[Ref[T]]()
    measure(nIters) { i =>
      buf += action(i)
    }
    ctx.emit(name, buf.result(): _*)
  }

  lazy val l = toRep(10)
  lazy val r = toRep(10.toByte)
  lazy val lC = RCCostedPrim(l, 1, RCSizePrim(4L, element[Int]))
  lazy val rC = RCCostedPrim(r, 1, RCSizePrim(1L, element[Byte]))
  lazy val pC = RCCostedPair(lC, rC, 1)
  lazy val ppC = RCCostedPair(pC, pC, 1)

  ignore("dataSize of CostedPair") {
    val sizeD= pC.size
    val expected = RCSizePair(RCSizePrim(4L, element[Int]), RCSizePrim(1L, element[Byte]))
    sizeD shouldBe expected
  }

  ignore("dataSize of nested CostedPair") {
    val sizeD= ppC.size
    val ppSize = pC.size
    val expected  = RCSizePair(ppSize, ppSize)
    sizeD shouldBe expected
  }

  val Colls = new special.collection.CollOverArrayBuilder
  val xs = Colls.fromItems(10, 20, 30)
  lazy val xsSym: Ref[Coll[Int]] = liftConst(xs)
  lazy val xsCosts = liftConst(Colls.replicate(3, 0))
  lazy val IntSize: RSize[Int] = costedBuilder.mkSizePrim(4L, element[Int])
  lazy val xsSizes = colBuilder.replicate(3, IntSize)
  lazy val xsC = costedBuilder.mkCostedColl(xsSym, xsCosts, xsSizes, 0)

  test("dataSize of CostedColl") {
    val sizeD = xsC.size
    val expected = RCSizeColl(xsSizes)
    sizeD shouldBe expected
  }

  val opt: Option[Int] = Some(10)
  lazy val optSym = liftConst(opt)
  lazy val optSize = RWSpecialPredef.some(IntSize)
  lazy val optC = costedBuilder.mkCostedOption(optSym, RWSpecialPredef.some(0), optSize, 0)

  test("dataSize of CostedOption") {
    val sizeD = optC.size
    val expected = RCSizeOption(optSize)
    sizeD shouldBe expected
  }

}
