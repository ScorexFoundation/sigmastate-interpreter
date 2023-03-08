package thelastpackage

import org.scalatest.BeforeAndAfterAll
import org.scalatest.propspec.AnyPropSpec
import sigmastate.ReflectionGenerator

class ZLastSpec extends AnyPropSpec with BeforeAndAfterAll {

  property("the last property") {

  }

  override protected def afterAll(): Unit = {
// uncomment to see the report
//    println(ReflectionGenerator.generateReport)
  }
}
