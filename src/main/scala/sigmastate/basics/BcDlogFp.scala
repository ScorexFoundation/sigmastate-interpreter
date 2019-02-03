package sigmastate.basics

import java.math.BigInteger

import org.bouncycastle.asn1.x9.X9ECParameters
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.math.ec.custom.djb.Curve25519Point
import org.bouncycastle.math.ec.custom.sec.{SecP384R1Point, SecP521R1Point}
import org.bouncycastle.math.ec.ECPoint
import org.bouncycastle.util.BigIntegers

import scala.collection.mutable
import scala.util.Try


abstract class BcDlogFp[ElemType <: ECPoint](val x9params: X9ECParameters) extends DlogGroup[ElemType] {

  lazy val curve = x9params.getCurve

  //modulus of the field
  lazy val p: BigInteger = curve.getField.getCharacteristic

  //order of the group
  lazy val q = x9params.getN

  //Now that we have p, we can calculate k which is the maximum length in bytes
  // of a string to be converted to a Group Element of this group.
  lazy val k = calcK(p)

  /**
    * The class GroupElementExponentiations is a nested class of DlogGroupAbs.<p>
    * It performs the actual work of pre-computation of the exponentiations for one base.
    * It is composed of two main elements. The group element for which the optimized computations
    * are built for, called the base and a vector of group elements that are the result of
    * exponentiations of order 1,2,4,8,
    */
  private class GroupElementsExponentiations(base: ElemType) //group element for which the optimized computations are built for
  /**
    * The constructor creates a map structure in memory.
    * Then calculates the exponentiations of order 1,2,4,8 for the given base and save them in the map.
    *
    * @param base
    * @throws IllegalArgumentException
    */ { // build new vector of exponentiations

    private val exponentiations = new mutable.ListBuffer[ElemType]()

    exponentiations += this.base // add the base - base^1
    val two = new BigInteger("2")
    (1 until 4).foreach { i =>
      exponentiations += exponentiate(exponentiations(i - 1), two)
    }

    /**
      * Calculates the necessary additional exponentiations and fills the exponentiations vector with them.
      *
      * @param size - the required exponent
      * @throws IllegalArgumentException
      */
    private def prepareExponentiations(size: BigInteger): Unit = { //find log of the number - this is the index of the size-exponent in the exponentiation array
      val index = size.bitLength - 1
      /* calculates the necessary exponentiations and put them in the exponentiations vector */
      (exponentiations.size to index).foreach { i =>
        exponentiations += exponentiate(exponentiations(i - 1), two)
      }
    }

    /**
      * Checks if the exponentiations had already been calculated for the required size.
      * If so, returns them, else it calls the private function prepareExponentiations with the given size.
      *
      * @param size - the required exponent
      * @return groupElement - the exponentiate result
      */
    def getExponentiation(size: BigInteger): ElemType = {
      /**
        * The exponents in the exponents vector are all power of 2.
        * In order to achieve the exponent size, we calculate its closest power 2 in the exponents vector
        * and continue the calculations from there.
        */
      // find the the closest power 2 exponent
      val index = size.bitLength - 1

      /* if the requested index out of the vector bounds, the exponents have not been calculated yet, so calculates them.*/
      if (exponentiations.lengthCompare(index) <= 0) prepareExponentiations(size)

      var exponent = exponentiations(index) //get the closest exponent in the exponentiations vector

      /* if size is not power 2, calculates the additional multiplications */
      val lastExp = two.pow(index)
      val difference = size.subtract(lastExp)
      if (difference.compareTo(BigInteger.ZERO) > 0) {
        val diff = getExponentiation(size.subtract(lastExp))
        exponent = multiplyGroupElements(diff, exponent)
      }
      exponent
    }
  }

  //map for multExponentiationsWithSameBase calculations
  private val exponentiationsCache = mutable.Map[ElemType, GroupElementsExponentiations]()


  //Create the generator
  //Assume that (x,y) are the coordinates of a point that is indeed a generator but check that (x,y) are the coordinates of a point.
  override lazy val generator: ElemType = x9params.getG.asInstanceOf[ElemType]

  /**
    * Checks if the given x and y represent a valid point on the given curve,
    * i.e. if the point (x, y) is a solution of the curves equation.
    *
    * @param x coefficient of the point
    * @param y coefficient of the point
    * @return true if the given x and y represented a valid point on the given curve
    */
  def checkCurveMembership(x: BigInteger, y: BigInteger): Boolean = {
    Try(curve.validatePoint(x, y)).isSuccess
  }


  /**
    * This function calculates k, the maximum length in bytes of a string to be converted to a Group Element of this group.
    *
    * @param p
    * @return k
    */
  def calcK(p: BigInteger): Int = {
    val bitsInp = p.bitLength
    var k = Math.floor((0.4 * bitsInp) / 8).toInt - 1
    //For technical reasons of how we chose to do the padding for encoding and decoding (the least significant byte of the encoded string contains the size of the
    //the original binary string sent for encoding, which is used to remove the padding when decoding) k has to be <= 255 bytes so that the size can be encoded in the padding.
    if (k > 255) k = 255
    k
  }

  /**
    * This function returns the k least significant bytes of the number x
    *
    * @param x
    * @param k
    * @return k least significant bits of x
    */
  def getKLeastSignBytes(x: BigInteger, k: Int): Array[Byte] = { //To retrieve the k least significant bits of a number x we do:
    //lsb = x mod (2^8k)
    val modulo = BigInteger.valueOf(2).pow(8 * k)
    x.mod(modulo).toByteArray
  }

  def checkMembershipAndCreate(x: BigInteger, y: BigInteger): Try[ElemType] = Try {
    val valid = checkCurveMembership(x, y)
    // checks validity
    if (!valid) throw new IllegalArgumentException("x, y values are not a point on this curve")
    curve.validatePoint(x, y).asInstanceOf[ElemType]
  }

  /**
    *
    * @return the order of this Dlog group
    */
  override lazy val order: BigInteger = x9params.getN

  /**
    *
    * @return the identity of this Dlog group
    */
  override lazy val identity: ElemType = curve.getInfinity.asInstanceOf[ElemType]


  def createPoint(x: BigInteger, y: BigInteger): ElemType = curve.createPoint(x, y).asInstanceOf[ElemType]


  /**
    * Creates ECPoint.Fp with infinity values
    */
  lazy val getInfinity: ElemType = curve.getInfinity.asInstanceOf[ElemType]


  /**
    * Calculates the inverse of the given GroupElement.
    *
    * @param groupElement to invert
    * @return the inverse element of the given GroupElement
    * @throws IllegalArgumentException
    **/
  override def getInverse(groupElement: ElemType): ElemType =
    groupElement.negate().asInstanceOf[ElemType]

  /**
    * Raises the base GroupElement to the exponent. The result is another GroupElement.
    *
    * @param exponent
    * @param base
    * @return the result of the exponentiation
    * @throws IllegalArgumentException
    */
  override def exponentiate(base: ElemType, exponent: BigInteger): ElemType = {
    //infinity remains the same after any exponentiate
    if (base.isInfinity) return base

    //If the exponent is negative, convert it to be the exponent modulus q.
    val exp = if (exponent.compareTo(BigInteger.ZERO) < 0) exponent.mod(order) else exponent

    /*
     * BC treats EC as additive group while we treat that as multiplicative group.
     * Therefore, exponentiate point is multiply.
     */
    base.multiply(exp).asInstanceOf[ElemType]
  }


  /**
    * Creates a random member of this Dlog group
    *
    * @return the random element
    */
  override def createRandomElement(): ElemType = {
    //This is a default implementation that is valid for all the Dlog Groups and relies on mathematical properties of the generators.
    //However, if a specific Dlog Group has a more efficient implementation then is it advised to override this function in that concrete
    //Dlog group. For example we do so in CryptoPpDlogZpSafePrime.
    val one = BigInteger.ONE
    val qMinusOne = x9params.getN.subtract(one)
    // choose a random number x in Zq*
    val randNum = BigIntegers.createRandomInRange(one, qMinusOne, secureRandom)
    // compute g^x to get a new element
    exponentiate(generator, randNum)
  }


  /**
    *
    * Checks if the order of this group is greater than `2^numBits`
    * @param numBits
    * @return <code>true</code> if the order is greater than `2^numBits`;<p>
    *         <code>false</code> otherwise.
    **/
  override def orderGreaterThan(numBits: Int): Boolean =
    if (order.compareTo(new BigInteger("2").pow(numBits)) > 0) true else false

  /**
    * Multiplies two GroupElements
    *
    * @param groupElement1
    * @param groupElement2
    * @return the multiplication result
    * @throws IllegalArgumentException
    */
  override def multiplyGroupElements(groupElement1: ElemType, groupElement2: ElemType): ElemType =
    groupElement1.add(groupElement2).asInstanceOf[ElemType]


  /**
    * Computes the product of several exponentiations with distinct bases
    * and distinct exponents.
    * Instead of computing each part separately, an optimization is used to
    * compute it simultaneously.
    *
    * @param groupElements
    * @param exponentiations
    * @return the exponentiation result
    */
  override def simultaneousMultipleExponentiations(groupElements: Array[ElemType],
                                                   exponentiations: Array[BigInteger]): ElemType =
    computeLL(groupElements, exponentiations)

  /**
    * Computes the product of several exponentiations of the same base
    * and distinct exponents.
    * An optimization is used to compute it more quickly by keeping in memory
    * the result of h1, h2, h4,h8,... and using it in the calculation.<p>
    * Note that if we want a one-time exponentiation of h it is preferable to use the basic exponentiation function
    * since there is no point to keep anything in memory if we have no intention to use it.
    *
    * @param base
    * @param exponent
    * @return the exponentiation result
    */
  override def exponentiateWithPreComputedValues(base: ElemType, exponent: BigInteger): ElemType = {
    //extracts from the map the GroupElementsExponentiations object corresponding to the accepted base
    val exponentiations = exponentiationsCache.getOrElse(key = base, {
      // if there is no object that matches this base - create it and add it to the map
      val exps = new GroupElementsExponentiations(base)
      exponentiationsCache.put(base, exps)
      exps
    })

    // calculates the required exponent
    exponentiations.getExponentiation(exponent)
  }

  /**
    * This function cleans up any resources used by exponentiateWithPreComputedValues for the requested base.
    * It is recommended to call it whenever an application does not need to continue calculating exponentiations for this specific base.
    *
    * @param base
    */
  override def endExponentiateWithPreComputedValues(base: ElemType): Unit = exponentiationsCache -= base

  /**
    * This function returns the value <I>k</I> which is the maximum length of a string to be encoded to a Group Element of this group.<p>
    * Any string of length <I>k</I> has a numeric value that is less than (p-1)/2 - 1.
    * <I>k</I> is the maximum length a binary string is allowed to be in order to encode the said binary string to a group element and vice-versa.<p>
    * If a string exceeds the <I>k</I> length it cannot be encoded.
    *
    * @return k the maximum length of a string to be encoded to a Group Element of this group. k can be zero if there is no maximum.
    */
  override lazy val maxLengthOfByteArrayForEncoding: Int = k


  /*
	 * Computes the simultaneousMultiplyExponentiate using a naive algorithm
	 */
  protected def computeNaive(groupElements: Array[ElemType], exponentiations: Array[BigInteger]): ElemType =
    groupElements.zip(exponentiations)
      .map { case (base, exp) => exponentiate(base, exp) }
      .foldLeft(identity) { case (r, elem) => multiplyGroupElements(elem, r) }


  /*
   * Compute the simultaneousMultiplyExponentiate by LL algorithm.
   * The code is taken from the pseudo code of LL algorithm in http://dasan.sejong.ac.kr/~chlim/pub/multi_exp.ps.
   */
  protected def computeLL(groupElements: Array[ElemType], exponentiations: Array[BigInteger]): ElemType = {
    val n = groupElements.length
    //get the biggest exponent
    val bigExp = exponentiations.max
    val t = bigExp.bitLength //num bits of the biggest exponent.
    val w = getLLW(t) //window size, choose it according to the value of t

    //h = n/w
    val h = if ((n % w) == 0) n / w else (n / w) + 1

    //create pre computation table
    val preComp = createLLPreCompTable(groupElements, w, h)

    //holds the computation result
    var result: ElemType = computeLoop(exponentiations, w, h, preComp, identity, t - 1)
    //computes the first loop of the algorithm. This loop returns in the next part of the algorithm with one single tiny change.

    //computes the third part of the algorithm
    (t - 2).to(0, -1).foreach { j =>
      //Y = Y^2
      result = exponentiate(result, new BigInteger("2"))
      //computes the inner loop
      result = computeLoop(exponentiations, w, h, preComp, result, j)
    }
    result
  }

  /*
   * Computes the loop the repeats in the algorithm.
   * for k=0 to h-1
   * 		e=0
   * 		for i=kw to kw+w-1
   *			if the bitIndex bit in ci is set:
   *			calculate e += 2^(i-kw)
   *		result = result *preComp[k][e]
   *
   */
  private def computeLoop(exponentiations: Array[BigInteger], w: Int, h: Int, preComp: Seq[Seq[ElemType]], result: ElemType, bitIndex: Int) = {
    var res = result
    (0 until h).foreach { k =>
      var e = 0
      (k * w until (k * w + w)).foreach { i =>
        if (i < exponentiations.length) { //if the bit is set, change the e value
          if (exponentiations(i).testBit(bitIndex)) {
            val twoPow = Math.pow(2, i - k * w).toInt
            e += twoPow
          }
        }
      }
      res = multiplyGroupElements(res, preComp(k)(e))
    }
    res
  }

  /*
   * Creates the preComputation table.
   */
  private def createLLPreCompTable(groupElements: Array[ElemType], w: Int, h: Int) = {
    val twoPowW = Math.pow(2, w).toInt
    //create the pre-computation table of size h*(2^(w))
    val preComp: Seq[mutable.Seq[ElemType]] = Seq.fill(h)(mutable.Seq.fill(twoPowW)(identity))

    (0 until h).foreach { k =>
      (0 until twoPowW).foreach { e =>
        (0 until w).foreach { i =>
          val baseIndex = k * w + i
          if (baseIndex < groupElements.length) {
            val base = groupElements(baseIndex)
            //if bit i in e is set, change preComp[k][e]
            if ((e & (1 << i)) != 0) { //bit i is set
              preComp(k)(e) = multiplyGroupElements(preComp(k)(e), base)
            }
          }
        }
      }
    }
    preComp
  }

  /*
   * returns the w value according to the given t
   */
  private def getLLW(t: Int): Int = {
    if (t <= 10) 2
    else if (t <= 24) 3
    else if (t <= 60) 4
    else if (t <= 144) 5
    else if (t <= 342) 6
    else if (t <= 797) 7
    else if (t <= 1828) 8
    else 9
  }
}

object SecP384R1 extends BcDlogFp[SecP384R1Point](CustomNamedCurves.getByName("secp384r1")) with App {
  val elems = 5000
  val base = generator
  val exps = (1 to elems).map { _ =>
    val one = BigInteger.ONE
    val qMinusOne = x9params.getN.subtract(one)
    // choose a random number x in Zq*
    BigIntegers.createRandomInRange(one, qMinusOne, secureRandom)
  }.toArray

  println(exps.map(e => exponentiateWithPreComputedValues(base, e) == exponentiate(base, e)).forall(_ == true))

  var t0 = System.currentTimeMillis()
  exps.foreach(exp => exponentiate(base, exp))
  println(System.currentTimeMillis() - t0)


  t0 = System.currentTimeMillis()
  exps.foreach(exp => exponentiateWithPreComputedValues(base, exp))
  println(System.currentTimeMillis() - t0)
}

object SecP521R1 extends BcDlogFp[SecP521R1Point](CustomNamedCurves.getByName("secp521r1")) with App {
  val elems = 1000
  val bases = (1 to elems).map(_ => createRandomGenerator()).toArray
  val exps = (1 to elems).map { _ =>
    val one = BigInteger.ONE
    val qMinusOne = x9params.getN.subtract(one)
    // choose a random number x in Zq*
    BigIntegers.createRandomInRange(one, qMinusOne, secureRandom)
  }.toArray

  var t0 = System.currentTimeMillis()
  val naive = computeNaive(bases, exps)
  println(System.currentTimeMillis() - t0)

  t0 = System.currentTimeMillis()
  val ll = computeLL(bases, exps)
  println(System.currentTimeMillis() - t0)

  println(naive.normalize().getAffineXCoord)
  println(naive.normalize().getAffineYCoord)

  println(ll.normalize().getAffineXCoord)
  println(ll.normalize().getAffineYCoord)

  println(naive == ll)
}

object Curve25519 extends BcDlogFp[Curve25519Point](CustomNamedCurves.getByName("curve25519"))