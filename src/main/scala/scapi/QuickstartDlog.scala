package scapi

import java.math.BigInteger
import java.security.SecureRandom

import edu.biu.scapi.primitives.dlog.openSSL.OpenSSLDlogECF2m
import org.bouncycastle.util.BigIntegers


object QuickstartDlog extends App {
  System.setProperty("java.library.path", System.getProperty("java.library.path") + ":/usr/lib/scapi")
  val sysPathsField = classOf[ClassLoader].getDeclaredField("sys_paths")
  sysPathsField.setAccessible(true)
  sysPathsField.set(null, null)
  //println(System.getProperty("java.library.path"))
  System.loadLibrary("OpenSSLJavaInterface")

  val dlog = new OpenSSLDlogECF2m("K-233")
  val random = new SecureRandom()

  // get the group generator and order
  val g = dlog.getGenerator

  println(dlog.getGenerator)
  println(dlog.getGenerator)
  println(dlog.getGenerator)
  println(dlog.getGenerator)

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



