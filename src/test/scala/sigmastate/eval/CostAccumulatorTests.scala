package sigmastate.eval

import scalan.Lazy
import sigmastate.helpers.SigmaTestingCommons

/** This test cases specify evaluation semantics of OpCost operation. */
class CostAccumulatorTest extends SigmaTestingCommons { suite =>

  lazy val IR = new TestingIRContext()
  import IR._
  import Liftables._

  /** Take graph building lambda, compile it and apply to the given arg */
  def run[ST, T](name: String, fLam: Rep[T] => Rep[Int], x: ST, expectedRes: Int)(implicit lT: Liftable[ST,T]): Unit = {
    val fSym = fun(fLam)(Lazy(lT.eW))
    emit(name, fSym) // save graph to file
    val f = IR.compile(getDataEnv, fSym, None)
    val (y, _) = f(x)
    y shouldBe expectedRes
  }

  def run2[ST, T, SU, U](name: String,
                         fLam: (Rep[T], Rep[U]) => Rep[Int], x: (ST, SU), expectedRes: Int)
                        (implicit lT: Liftable[ST, T], lU: Liftable[SU, U]): Unit = {
    implicit val eT = lT.eW
    implicit val eU = lU.eW
    val fSym = fun((p: Rep[(T,U)]) => fLam(p._1, p._2))
    emit(name, fSym) // save graph to file
    val f = IR.compile(getDataEnv, fSym, None)
    val (y, _) = f(x)
    y shouldBe expectedRes
  }

  lazy val v1 = variable[Int]
  lazy val v2 = variable[Int]

  property("CostAccumulator single OpCode") {
    run("opCost_const", { _: Rep[Int] => opCost(v1, Nil, 5) }, 10, 5)
    run("opCost_arg_const", { _: Rep[Int] => opCost(v1, Seq(5), 5) }, 10, 5)
    run("opCost_arg_and_const", { _: Rep[Int] => opCost(v1, Seq(5), 10) }, 10, 15)

    run("opCost_id", { x: Rep[Int] => opCost(v1, Nil, x) }, 10, 10)
    run("opCost_const_id", { x: Rep[Int] => opCost(v1, Seq(5), x) }, 10, 15)
    run2("opCost_const_id2", { (x: Rep[Int], y: Rep[Int]) => opCost(v1, Seq(y), x) }, (10, 5), 15)

    run("opCost_id_const", { x: Rep[Int] => opCost(v1, Seq(x), 6) }, 10, 16)
    run2("opCost_const_id2", { (x: Rep[Int], y: Rep[Int]) => opCost(v1, Seq(x), y) }, (10, 6), 16)

    run("opCost_arg_id", { x: Rep[Int] => opCost(v1, Seq(x), x) }, 10, 10)

    run("opCost_two_args", { x: Rep[Int] => opCost(v1, Seq(x, x), 5) }, 10, 15)
    run2("opCost_two_args_2", { (x: Rep[Int], y: Rep[Int]) => opCost(v1, Seq(x, x), y) }, (10, 5), 15)
    run2("opCost_two_args_3", { (x: Rep[Int], y: Rep[Int]) => opCost(v1, Seq(x, y), y) }, (10, 5), 15)
    run2("opCost_two_args_4", { (x: Rep[Int], y: Rep[Int]) => opCost(v1, Seq(x, y), x + y) }, (10, 5), 30)
  }

  property("CostAccumulator two OpCode") {
    run("2xOpCost_1", { _: Rep[Int] =>
      val c1 = opCost(v1, Nil, 5)
      val c2 = opCost(v2, Nil, 5)
      c1 + c2
      }, 10, 15)
    run("2xOpCost_2", { x: Rep[Int] =>
      val c1 = opCost(v1, Nil, x)
      val c2 = opCost(v2, Nil, x)
      c1 + c2
      }, 10, 30)
    run("2xOpCost_3", { x: Rep[Int] =>
      val c1 = opCost(v2, Nil, x)
      opCost(v1, Seq(c1), x)
      }, 10, 20)
  }

}
