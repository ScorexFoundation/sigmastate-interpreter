package scapi

import java.math.BigInteger
import java.security.SecureRandom

import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import org.bouncycastle.util.BigIntegers


object QuickstartDlog extends App {

  val dlog = new BcDlogECFp()
  val random = new SecureRandom()

  // get the group generator and order
  val g = dlog.getGenerator

  println(s"generator: ${dlog.getGenerator}")

  val q = dlog.getOrder
  val qMinusOne = q.subtract(BigInteger.ONE)

  // create a random exponent r
  val r = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, random)

  // exponentiate g in r to receive a new group element
  val g1 = dlog.exponentiate(g, r)

  // create a random group element
  val h = dlog.createRandomElement()
  // multiply elements
  val gMult = dlog.multiplyGroupElements(g1, h)

  println(gMult)
}
