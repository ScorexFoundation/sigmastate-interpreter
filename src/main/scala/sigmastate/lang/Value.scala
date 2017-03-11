package sigmastate.lang


sealed trait Value[V, VT <: Value[V, VT]] {
  val value: V
}


case class StringValue(override val value: String) extends Value[String, StringValue]
case class IntValue(override val value: Int) extends Value[Int, IntValue]
case class BooleanValue(override val value: Boolean) extends Value[Boolean, BooleanValue]
case class NonNegValue(override val value: Int) extends Value[Int, NonNegValue] {
  require(value >= 0)
}

//todo: types for non-negative ints, merkle proofs

trait Variable[V, VT <: Value[V, VT]] extends Value[V, VT]{
  def name: String
}

trait Operation[V1, VT1 <: Value[V1, VT1], V2, VT2 <: Value[V2, VT2], V3, VT3 <: Value[V3, VT3] ] {
  val v1: VT1
  val v2: VT2

  def eval(): VT3
}

trait SimpleOperation[V, VT <: Value[V, VT]] extends Operation[V, VT, V, VT, V, VT]

trait Relation[V1, VT1 <: Value[V1, VT1], V2, VT2 <: Value[V2, VT2]] {
  val v1: VT1
  val v2: VT2

  def holds: Boolean
}

trait SimpleRelation[V, VT <: Value[V, VT]] extends Relation[V, VT, V, VT]


package object nonNeg {
  case class LT (override val v1: NonNegValue, v2: NonNegValue) extends SimpleRelation[Int, NonNegValue]{
    override lazy val holds = v1.value < v2.value
  }
}

package object blockchain {
  case class Height(h: Int) extends Variable[Int, NonNegValue]{
    def name = "blockchain.height"
    val value = h
  }
}

object test extends App{
  import nonNeg._

  LT(NonNegValue(2), NonNegValue(3))

}

