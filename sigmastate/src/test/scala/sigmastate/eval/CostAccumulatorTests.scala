package sigmastate.eval

import org.scalatest.exceptions.TestFailedException
import scalan.Lazy
import sigmastate.helpers.SigmaTestingCommons

/** This test cases specify evaluation semantics of OpCost operation. */
class CostAccumulatorTest extends SigmaTestingCommons { suite =>

  lazy val IR = new TestingIRContext() {
    override val okPrintEvaluatedEntries = true
  }
  import IR._
  import Liftables._

  /** Take graph building lambda, compile it and apply to the given arg */
  def run[ST, T](name: String, fLam: Ref[T] => Ref[Int], x: ST, expectedRes: Int)(implicit lT: Liftable[ST,T]): Unit = {
    val fSym = fun(fLam)(Lazy(lT.eW))
    emit(name, fSym) // save graph to file
    val f = IR.compile(getDataEnv, fSym, None)
    val (y, _) = f(x)
    y shouldBe expectedRes
  }

  def run2[ST, T, SU, U](name: String,
                         fLam: (Ref[T], Ref[U]) => Ref[Int], x: (ST, SU), expectedRes: Int)
                        (implicit lT: Liftable[ST, T], lU: Liftable[SU, U]): Unit = {
    implicit val eT = lT.eW
    implicit val eU = lU.eW
    val fSym = fun((p: Ref[(T,U)]) => fLam(p._1, p._2))
    emit(name, fSym) // save graph to file
    val f = IR.compile(getDataEnv, fSym, None)
    val (y, _) = f(x)
    y shouldBe expectedRes
  }

  lazy val v1 = variable[Int]
  lazy val v2 = variable[Int]
  lazy val v3 = variable[Int]

  /** How to read test cases:
    * In the tests  `cost` refers to CostAccumulator.currentScope.currentCost
    * cost = 0  at the beginning of all lambdas
    * + - means the value is added to the current cost
    * M - the node is marked as visited, and repeated additions are avoided.
    * S - the value is skipped and not added to the current cost
    */
  property("CostAccumulator single OpCode") {
    run("opCost_const", { _: Ref[Int] =>
      opCost(v1, Nil, 5/*+,M*/)
      }, 0/*not used*/, 5)    // cost = 5

    run("opCost_arg_const", { _: Ref[Int] =>
      opCost(v1, Seq(4/*+4,M*/), 6/*+6*/) // cost = 10
      }, 0/*not used*/, 10)

    run("opCost_arg_and_const", { _: Ref[Int] =>
      opCost(v1, Seq(5/*+,M*/), 10/*+*/)  // cost = 15
      }, 0/*not used*/, 15)

    run("opCost_id", { x: Ref[Int] => opCost(v1, Nil, x/*+*/) }, 10, 10)
    run("opCost_const_id", { x: Ref[Int] => opCost(v1, Seq(5/*+,M*/), x/*+*/) }, 10, 15)
    run2("opCost_const_id2", { (x: Ref[Int], y: Ref[Int]) => opCost(v1, Seq(y), x) }, (10, 5), 15)

    run("opCost_id_const", { x: Ref[Int] => opCost(v1, Seq(x), 6) }, 10, 16)
    run2("opCost_const_id2", { (x: Ref[Int], y: Ref[Int]) => opCost(v1, Seq(x), y) }, (10, 6), 16)

    an[TestFailedException] should be thrownBy run("opCost_arg_id", { x: Ref[Int] => opCost(v1, Seq(x), x) }, 10, 10)

    run("opCost_two_args", { x: Ref[Int] => opCost(v1, Seq(x, x), 5) }, 10, 15)
    run2("opCost_two_args_2", { (x: Ref[Int], y: Ref[Int]) => opCost(v1, Seq(x, x), y) }, (10, 5), 15)

    an[TestFailedException] should be thrownBy run2("opCost_two_args_3", { (x: Ref[Int], y: Ref[Int]) => opCost(v1, Seq(x, y), y) }, (10, 5), 15)

    run2("opCost_two_args_4", { (x: Ref[Int], y: Ref[Int]) => opCost(v1, Seq(x, y), x + y) }, (10, 5), 30)
  }

  property("CostAccumulator two OpCode") {
    run("2xOpCost_1", { _: Ref[Int] =>
      val c0: Ref[Int] = 5   // const node
      val c1 = opCost(v1, Nil, c0/*+*/) // cost = 5
      val c2 = opCost(v2, Nil, c0/*+*/) // cost = 10
      c1 + c2
      }, 10, 15)
    run("2xOpCost_2", { x: Ref[Int] =>
      val c1 = opCost(v1, Nil, x/*+*/) // cost = 10
      val c2 = opCost(v2, Nil, x/*+*/) // cost = 20
      c1 + c2
      }, 10, 30)
    run("2xOpCost_3", { x: Ref[Int] =>
      val c1 = opCost(v1, Nil, x/*+*/) // cost = 10, c1 marked
      opCost(v2, Seq(c1), x/*+*/)      // cost = 20
      }, 10, 20)
    run("2xOpCost_4", { x: Ref[Int] =>
      val c1 = opCost(v1, Nil, x/*+*/) // cost = 10, c1 marked
      opCost(v2, Nil, c1)              // cost = 10 because c1 is marked
      }, 10, 10)
    run("2xOpCost_5", { x: Ref[Int] =>
      val c0: Ref[Int] = 5   // const node
      val c1 = opCost(v1, Seq(c0/*+,M*/), x/*+*/) // cost = 15
      opCost(v2, Nil, c1)                           // cost = 15 because c1 is marked
      }, 10, 15)
    run2("2xOpCost_6", { (x: Ref[Int], y: Ref[Int]) =>
      val c1 = opCost(v1, Seq(y/*+,M*/), x/*+*/)  // cost = 15
      opCost(v2, Nil, c1)                           // cost = 15 because c1 is marked
      }, (10, 5), 15)

    run2("2xOpCost_7", { (x: Ref[Int], y: Ref[Int]) =>
      val c1 = opCost(v1, Seq(y/*+,M*/), x/*+*/)  // cost = 15
      opCost(v2, Seq(x/*+,M*/), c1/*S*/)          // cost = 25 because x is not marked
      }, (10, 5), 25)
  }

  property("CostAccumulator three OpCode") {
    run2("3xOpCost_1", { (x: Ref[Int], y: Ref[Int]) =>
      val c0: Ref[Int] = 5   // const node
      val o1 = opCost(v1, Seq(x/*+10,M*/), c0/*+5*/)         // cost = 15
      val o2 = opCost(v2, Seq(c0/*+5,M*/, x/*S*/, o1/*S*/), y/*+5*/)  // cost = 25
      opCost(v3, Seq(y/*+5,M*/, o2/*S*/), c0/*S*/)          // cost = 30
      }, (10, 5), 30)

    // o1 is used in OpCost.args
    run2("3xOpCost_2", { (x: Ref[Int], _: Ref[Int]) =>
      val c0: Ref[Int] = 5   // const node
      val o1 = opCost(v1, Nil, c0/*+5*/)            // cost = 5
      val o2 = opCost(v2, Seq(o1/*S*/), x/*+10*/)  // cost = 15
      val o3 = opCost(v3, Seq(o1/*S*/, o2/*S*/), c0/*+5*/) // cost = 20
      o3 * 3
      }, (10, 5), 60)

    // same as above but using y
    run2("3xOpCost_3", { (x: Ref[Int], y: Ref[Int]) =>
      val c0: Ref[Int] = 5   // const node
      val o1 = opCost(v1, Nil, c0/*+5*/)           // cost = 5
      val o2 = opCost(v2, Seq(o1/*S*/), x/*+10*/)  // cost = 15
      val o3 = opCost(v3, Seq(o1/*S*/, o2/*S*/), y/*+5*/) // cost = 20
      o3 * 3
      }, (10, 5), 60)

    // o1 is used in outside OpCost
    run2("3xOpCost_4", { (x: Ref[Int], y: Ref[Int]) =>
      val c0: Ref[Int] = 5   // const node
      val o1 = opCost(v1, Nil, c0/*+5*/) // cost = 5, value = 5
      val o2 = opCost(v2, Seq(o1/*S*/), x/*+10*/)  // cost = 15
      val o3 = opCost(v3, Seq(o1/*S*/, o2/*S*/), y/*+5*/) // cost = 20
      o3 * o1  // == 20 * 5
      }, (10, 5), 100)

    // same as above but o1 is used in OpCost.opCost
    run2("3xOpCost_5", { (x: Ref[Int], y: Ref[Int]) =>
      val c0: Ref[Int] = 5   // const node
      val o1 = opCost(v1, Nil, c0/*+5*/) // cost = 5, value = 5
      val o2 = opCost(v2, Seq(o1/*S*/), x/*+10*/)  // cost = 15
      val o3 = opCost(v3, Seq(o1/*S*/, o2/*S*/), y + o1/*+10*/) // cost = 25
      o3 * o1  // == 25 * 5
      }, (10, 5), 125)

  }

  property("unfolding lambda into thunk") {
    // the following two tests check cost equivalence under lambda unfolding
    // with unfolding
    run("lambda_1", { y: Ref[Int] =>
      val c0: Ref[Int] = 5   // const node
      val c1: Ref[Int] = 6   // const node
      val f1 = fun { x: Ref[Int] =>
        opCost(v1, Seq(x/*+5,M*/, y/*+5,M*/), c1/*+6*/)  // cost = 16
      }
      // unfold f1 into thunk (this is staging time operation, there is no lambda in the final graph)
      val t1: Ref[Int] = ThunkForce(Thunk(f1(c0)))  // value = 16
      opCost(v2, Seq(t1/*+16,M*/, y/*+5*/), c0/*+5*/)         // cost = 26
      }, 5, 26)

    // without unfolding f1
    run("lambda_2", { y: Ref[Int] =>
      val c0: Ref[Int] = 5   // const node
      val f1 = fun { x: Ref[Int] =>
        opCost(v1, Seq(x/*+5,M*/, y/*+5,M*/), c0/*+5*/)  // cost = 15
      }
      // reify application without unfolding f1 (the lambda is called at runtime)
      val t1: Ref[Int] = Apply(f1, c0, false)  // value = 15
      opCost(v2, Seq(t1/*+15,M*/, y/*+5*/), c0/*+5*/)         // cost = 25
      }, 5, 25)
  }

  property("unfolding collision") {
    // the following two tests check cost equivalence under lambda unfolding
    // with unfolding
    run("collision_1", { y: Ref[Int] =>
      val c0: Ref[Int] = 5   // const node
      val f1 = fun { x: Ref[Int] =>
        opCost(v1, Seq(x/*+5,M*/, c0/*S*/), y/*+5*/)  // cost = 10, c0 is skipped due to collision with x after unfold
      }
      // unfold f1 into thunk (this is staging time operation, there is no lambda in the final graph)
      val t1: Ref[Int] = ThunkForce(Thunk(f1(c0)))  // value = 10
      opCost(v2, Seq(t1/*+10,M*/, y/*+5*/), c0/*+5*/)         // cost = 20
    }, 5, 20)

    // without unfolding f1
    run("collision_2", { y: Ref[Int] =>
      val c0: Ref[Int] = 5   // const node
      val f1 = fun { x: Ref[Int] =>
        opCost(v1, Seq(x/*+5,M*/, c0/*+5,M*/), y/*+5*/)  // cost = 15
      }
      // reify application without unfolding f1 (the lambda is called at runtime)
      val t1: Ref[Int] = Apply(f1, c0, false)  // value = 15
      opCost(v2, Seq(t1/*+15,M*/, y/*+5*/), c0/*+5*/)         // cost = 25
      }, 5, 25)
  }
}
