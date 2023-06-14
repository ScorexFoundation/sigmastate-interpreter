package thelastpackage

import org.scalatest.BeforeAndAfterAll
import org.scalatest.propspec.AnyPropSpec

/** This spec is the last in queue for execution when running all the tests.
  * It is thus useful to do something after all the tests have been executed.
  * For example, some generators (like ReflectionGenerator) and Profiler collect
  * runtime information about the program execution.
  * This information can be printed out after all the tests have been executed.
  */
class ZLastSpec extends AnyPropSpec with BeforeAndAfterAll {

  property("the last property") {

  }

  override protected def afterAll(): Unit = {
// uncomment to see the report
//    println(ReflectionGenerator.generateReport)
  }
}
