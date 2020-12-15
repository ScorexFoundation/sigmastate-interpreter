package sigmastate.helpers

import org.scalatest.Matchers

import scala.annotation.tailrec

trait NegativeTesting extends Matchers {

  /** Checks that a [[Throwable]] is thrown and satisfies the given predicate.
    * @param fun  block of code to execute
    * @param assertion  expected assertion on the thrown exception
    * @param clue  added to the error message
    */
  def assertExceptionThrown(fun: => Any, assertion: Throwable => Boolean, clue: => String = ""): Unit = {
    try {
      fun
      fail("exception is expected")
    }
    catch {
      case e: Throwable =>
        if (!assertion(e))
          fail(
            s"""exception check failed on $e (root cause: ${rootCause(e)})
              |clue: $clue
              |trace:
              |${e.getStackTrace.mkString("\n")}}""".stripMargin)
    }
  }

  /** Returns the root cause of the chain of exceptions. */
  @tailrec
  final def rootCause(t: Throwable): Throwable =
    if (t.getCause == null) t
    else rootCause(t.getCause)

}
