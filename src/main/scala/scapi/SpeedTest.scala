package scapi

import java.math.BigInteger
import java.security.SecureRandom

import edu.biu.scapi.primitives.dlog.cryptopp.CryptoPpDlogZpSafePrime
import edu.biu.scapi.primitives.dlog.openSSL.OpenSSLDlogECF2m
import edu.biu.scapi.primitives.hash.cryptopp.CryptoPpSHA256
import org.bouncycastle.util.BigIntegers


object SpeedTest extends App {
  System.setProperty("java.library.path", System.getProperty("java.library.path") + ":/usr/lib/scapi")
  val sysPathsField = classOf[ClassLoader].getDeclaredField("sys_paths")
  sysPathsField.setAccessible(true)
  sysPathsField.set(null, null)
  //println(System.getProperty("java.library.path"))
  System.loadLibrary("OpenSSLJavaInterface")
  System.loadLibrary("CryptoPPJavaInterface")

  val random = new SecureRandom()

  val sha256 = new CryptoPpSHA256
  val msgToHash = new Array[Byte](32)
  random.nextBytes(msgToHash)

  val hashResult = new Array[Byte](32)
  //heat up
  (1 to 250000).foreach{i =>
    sha256.update(msgToHash, 0, msgToHash.length)
    sha256.hashFinal(hashResult, 0)
  }

  val toRun = 100000

  val hashTime0 = System.currentTimeMillis()
  (1 to toRun).foreach{i =>
    sha256.update(msgToHash, 0, msgToHash.length)
    sha256.hashFinal(hashResult, 0)
  }
  val hashDelta = System.currentTimeMillis() - hashTime0
  println("time to SHA256: " + hashDelta)


  val dlog = new CryptoPpDlogZpSafePrime(128)

  // get the group generator and order
  val g = dlog.getGenerator
  val q = dlog.getOrder
  val qMinusOne = q.subtract(BigInteger.ONE)

  // create a random exponent r
  val r = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, random)

  // exponentiate g in r to receive a new group element

  val expTime0 = System.currentTimeMillis()
  (1 to toRun).foreach { i =>
    dlog.exponentiate(g, r)
  }
  val expDelta = System.currentTimeMillis() - expTime0
  println("time to exp: " + expDelta)

}
