package sigmastate.basics

import java.math.BigInteger



/**
  * Group agnostic representation of an EC point, just two numbers. The main purpose of this data
  * object is to be sent over wire. During converting this representation into an actual EC point, membership
  * must be tested.
  * @param x
  * @param y
  */
case class GroupAgnosticEcElement(x: BigInteger, y: BigInteger)

