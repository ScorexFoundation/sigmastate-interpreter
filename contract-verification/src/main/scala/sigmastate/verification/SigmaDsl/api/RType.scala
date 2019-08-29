package sigmastate.verification.SigmaDsl.api

//import sigmastate.verification.AvlTree
import sigmastate.verification.SigmaDsl.api.collection.Coll
import stainless.annotation.{extern, ignore, pure}

import scala.reflect.ClassTag

trait RType[A] {
  @ignore
  def classTag: ClassTag[A]

  @ignore
  def name: String = this.toString

  /** Returns true is data size of `x: A` is the same for all `x`.
    * This is useful optimizations of calculating sizes of collections. */
  def isConstantSize: Boolean
}

object RType {
}

