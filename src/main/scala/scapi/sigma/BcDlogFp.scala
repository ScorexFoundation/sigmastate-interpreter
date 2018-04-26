package scapi.sigma

import java.math.BigInteger
import java.security.SecureRandom

import com.google.common.primitives.Shorts
import org.bouncycastle.asn1.x9.X9ECParameters
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.math.ec.custom.djb.Curve25519Point
import org.bouncycastle.math.ec.custom.sec.{SecP384R1Point, SecP521R1Point}
import org.bouncycastle.math.ec.{ECFieldElement, ECPoint}
import org.bouncycastle.util.BigIntegers
import sigmastate.serialization.ValueSerializer

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
      /* calculates the necessary exponentiations and put them in the exponentiations vector */ var i = exponentiations.size

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
  private val exponentiationsMap = mutable.Map[ElemType, GroupElementsExponentiations]()


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
    * This function finds the y coordinate of a point in the curve for a given x, if it exists.
    *
    * @param x
    * @return the y coordinate of point in the curve for a given x, if it exists
    *         else, null
    */
  def findYInCurveEquationForX(x: BigInteger): BigInteger = {
    /* get a, b, p from group params */
    val a = x9params.getCurve.getA.toBigInteger
    val b = x9params.getCurve.getB.toBigInteger
    // compute x^3
    val x3 = x.modPow(new BigInteger("3"), p)
    // compute x^3+ax+b
    val rightSide = x3.add(a.multiply(x)).add(b).mod(p)
    //try to compute y = square_root(x^3+ax+b)
    //If it exists return it
    //else, return null
    //We compute the square root via the ECFieldElement.Fp of Bouncy Castle, since BigInteger does not implement this function.
    //ECFieldElement.Fp ySquare = new ECFieldElement.Fp(params.getQ(), rightSide);
    val ySquare = new ECFieldElement.Fp(p, rightSide)
    //TODO I am not sure which one of the square roots it returns (if they exist). We need to check this!! (Yael)
    val y = ySquare.sqrt.asInstanceOf[ECFieldElement.Fp]
    if (y != null) y.toBigInteger.mod(p)
    else null
  }

  /**
    * This function receives any string of size up to k bytes (as returned by CalcK), finds the coordinates of the point that is the encoding of this binary string.
    *
    * @param binaryString
    * @throws IndexOutOfBoundsException if the length of the binary array to encode is longer than k
    * @return an FpPoint with the coordinates of the corresponding GroupElement point or null if could not find the encoding in reasonable time
    */
  def findPointRepresentedByByteArray(binaryString: Array[Byte]): Try[ElemType] = Try { //Pseudo-code:
    /*If the length of binaryString exceeds k then throw IndexOutOfBoundsException.

              Let L be the length in bytes of p

              Choose a random byte array r of length L - k - 2 bytes

              Prepare a string newString of the following form: r || binaryString || binaryString.length (where || denotes concatenation) (i.e., the least significant byte of newString is the length of binaryString in bytes)

              Convert the result to a BigInteger (bIString)

              Compute the elliptic curve equation for this x and see if there exists a y such that (x,y) satisfies the equation.

              If yes, return (x,y)

              Else, go back to step 3 (choose a random r etc.) up to 80 times (This is an arbitrary hard-coded number).

              If did not find y such that (x,y) satisfies the equation after 80 trials then return null.
         */
    if (binaryString.length > k) throw new IndexOutOfBoundsException("The binary array to encode is too long.")
    val l = p.bitLength / 8
    val randomArray = new Array[Byte](l - k - 2)
    //Create a random object and make it seed itself:
    val rand = new SecureRandom
    val newString = new Array[Byte](randomArray.length + 1 + binaryString.length)
    var counter = 0
    var y: BigInteger = null
    var x: BigInteger = null
    do {
      rand.nextBytes(randomArray)
      System.arraycopy(randomArray, 0, newString, 0, randomArray.length)
      System.arraycopy(binaryString, 0, newString, randomArray.length, binaryString.length)
      newString(newString.length - 1) = binaryString.length.toByte
      //Convert the result to a BigInteger (bIString)
      x = new BigInteger(newString)
      if (x.compareTo(BigInteger.ZERO) < 0) {
        val temp = x.toByteArray
        val t0 = temp(0)
        temp(0) = (-t0).toByte
        x = new BigInteger(temp)
      }
      //Compute the elliptic curve equation for this x and see if there exists a y such that (x,y) satisfies the equation.
      //If yes, return (x,y)
      //Else, go back to choose a random r etc.)
      y = findYInCurveEquationForX(x)
      counter += 1
    } while ( {
      (y == null) && (counter <= 80) // todo: magic number
    }) //we limit the amount of times we try to 80 which is an arbitrary number.

    //If found the correct y in reasonable time then return the (x,y) FpPoint
    if (y != null) curve.createPoint(x, y).asInstanceOf[ElemType]
    else throw new Exception("Had no success within a certain number of iterations in findPointRepresentedByByteArray")
  }


  /**
    * checks if the given point is in the given dlog group with the q prime order.
    * A point is in the group if it in the q-order group which is a sub-group of the Elliptic Curve.
    * Base assumption of this function is that checkCurveMembership function is already been called and returned true.
    *
    * @param point
    * @return true if the given point is in the given dlog group.
    */
  def checkSubGroupMembership(point: ElemType): Boolean = { //we assume that the point is on the curve group
    //get the cofactor of the group
    val h = x9params.getH
    //if the cofactor is 1 the sub-group is same as the elliptic curve equation which the point is in.
    if (h == BigInteger.ONE) return true
    val y = point.getYCoord.toBigInteger
    //if the cofactor is greater than 1, the point must have order q (same as the order of the group)
    //if the cofactor is 2 and the y coefficient is 0, the point has order 2 and is not in the group
    if (h == new BigInteger("2")) if (y == BigInteger.ZERO) return false
    else return true
    // if the cofactor is 3 and p^2 = p^(-1), the point has order 3 and is not in the group
    if (h == new BigInteger("3")) {
      val power = exponentiate(point, new BigInteger("2"))
      val inverse = getInverse(point)
      if (power == inverse) return false
      else return true
    }
    // if the cofactor is 4, the point has order 2 if the y coefficient of the point is 0,
    // or the the point has order 4 if the y coefficient of the point raised to two is 0.
    // in both cases the point is not in the group.
    if (h == new BigInteger("4")) {
      if (y == BigInteger.ZERO) return false
      val power = exponentiate(point, new BigInteger("2"))
      val powerY = power.getYCoord.toBigInteger
      if (powerY == BigInteger.ZERO) return false
      else return true
    }
    // if the cofactor is bigger than 4, there is no optimized way to check the order, so we operates the naive:
    // if the point raised to q (order of the group) is the identity, the point has order q too and is in the group.
    // else, it is not in the group
    val r = q
    val pointPowR = exponentiate(point, r)
    if (pointPowR.isInfinity) true else false
  }


  /**
    * This function maps any group element to a byte array. This function does not have an inverse,<p>
    * that is, it is not possible to re-construct the original group element from the resulting byte array.
    *
    * @param x coordinate of a point in the curve (this function does not check for membership)
    * @param y coordinate of a point in the curve (this function does not check for membership)
    * @return byte[] representation
    */
  def mapAnyGroupElementToByteArray(x: BigInteger, y: BigInteger): Array[Byte] = { //This function simply returns an array which is the result of concatenating
    //the byte array representation of x with the byte array representation of y.
    val xByteArray = x.toByteArray
    val yByteArray = y.toByteArray
    val result = new Array[Byte](xByteArray.length + yByteArray.length)
    System.arraycopy(xByteArray, 0, result, 0, xByteArray.length)
    System.arraycopy(yByteArray, 0, result, xByteArray.length, yByteArray.length)
    result
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


  /**
    * Checks if the given element is a member of this Dlog group
    *
    * @param point
    * @return true if the given element is member of this group; false, otherwise.
    * @throws IllegalArgumentException
    */
  override def isMember(point: ElemType): Boolean = {
    //infinity point is a valid member
    if (point.isInfinity) return true
    // A point (x, y) is a member of a Dlog group with prime order q over an Elliptic Curve if it meets the following two conditions:
    // 1)	P = (x,y) is a point in the Elliptic curve, i.e (x,y) is a solution of the curves equation.
    // 2)	P = (x,y) is a point in the q-order group which is a sub-group of the Elliptic Curve.
    // those two checks are done in two steps:
    // 1.	Checking that the point is on the curve, performed by checkCurveMembership
    // 2.	Checking that the point is in the Dlog group,performed by checkSubGroupMembership
    point.isValid && checkSubGroupMembership(point)
  }


  def generateElement(x: BigInteger, y: BigInteger): ElemType = { //Creates element with the given values.
    val point = checkMembershipAndCreate(x, y).get
    //if the element was created, it is a point on the curve.
    //checks if the point is in the sub-group, too.
    val valid = checkSubGroupMembership(point)
    //if the point is not in the sub-group, throw exception.
    if (!valid) throw new IllegalArgumentException("Could not generate the element. The given (x, y) is not a point in this Dlog group")
    point
  }


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
    val randNum = BigIntegers.createRandomInRange(one, qMinusOne, random)
    // compute g^x to get a new element
    exponentiate(generator, randNum)
  }

  /**
    * This function takes any string of length up to k bytes and encodes it to a Group Element.
    * k can be obtained by calling getMaxLengthOfByteArrayForEncoding() and it is calculated upon construction of
    * this group; it depends on the length in bits of p.
    * The encoding-decoding functionality is not a bijection, that is, it is a 1-1 function but is not onto.
    * Therefore, any string of length in bytes up to k can be encoded to a group element but not every group element
    * can be decoded to a binary string in the group of binary strings of length up to 2^k.
    * Thus, the right way to use this functionality is first to encode a byte array and then to decode it,
    * and not the opposite.
    *
    *
    * @param binaryString the byte array to convert
    * @throws IndexOutOfBoundsException if the length of the binary array to encode is longer than k
    * @return the created group Element or null if could not find the encoding in reasonable time
    */
  override def encodeByteArrayToGroupElement(binaryString: Array[Byte]): Try[ElemType] = {
    findPointRepresentedByByteArray(binaryString).map {fpPoint =>
      curve.importPoint(fpPoint).asInstanceOf[ElemType]
    }
  }

  /**
    * This function decodes a group element to a byte array. This function is guaranteed to work properly ONLY if
    * the group element was obtained as a result of
    * encoding a binary string of length in bytes up to k.
    * This is because the encoding-decoding functionality is not a bijection, that is, it is a 1-1 function
    * but is not onto. Therefore, any string of length in bytes up to k can be encoded to a group element but not any
    * group element can be decoded to a binary sting in the group of binary strings of length up to 2^k.
    *
    *
    * @param point the element to convert
    * @return the created byte array
    */
  override def decodeGroupElementToByteArray(point: ElemType): Array[Byte] = {
    val xByteArray = point.getXCoord.toBigInteger.toByteArray
    val bOriginalSize = xByteArray(xByteArray.length - 1)

    val b2 = new Array[Byte](bOriginalSize)
    System.arraycopy(xByteArray, xByteArray.length - 1 - bOriginalSize, b2, 0, bOriginalSize)
    return b2
  }

  /**
    * This function maps a group element of this dlog group to a byte array.<p>
    * This function does not have an inverse function, that is, it is not possible to re-construct the original
    * group element from the resulting byte array. Moreover, the implementation of this function is such that
    * for a given group element (point in the curve), the result of applying this function
    * (mapAnyGroupElementToByteArray) and the result of applying decodeGroupElementToByteArray are not equal.
    *
    * @return a byte array representation of the given group element
    */
  override def mapAnyGroupElementToByteArray(point: ElemType): Array[Byte] = {
    //This function simply returns an array which is the result of concatenating
    //The actual work is implemented in ECFpUtility since it is independent of the underlying library (BC, Miracl, or other)
    //If we ever decide to change the implementation there will only one place to change it.

    point.getEncoded(true)
  }


  /**
    *
    * Checks if the order of this group is greater than 2^numBits
    * @param numBits
    * @return <code>true</code> if the order is greater than 2^numBits;<p>
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
    * Reconstructs a GroupElement given the GroupElementSendableData data, which might have been received through a Channel open between the party holding this DlogGroup and
    * some other party.
    *
    * @param bCheckMembership whether to check that the data provided can actually reconstruct an element of this DlogGroup. Since this action is expensive it should be used only if necessary.
    * @param data             the GroupElementSendableData from which we wish to "reconstruct" an element of this DlogGroup
    * @return the reconstructed GroupElement
    */
  override def reconstructElement(bCheckMembership: Boolean, data: GroupAgnosticEcElement): Try[ElemType] = Try {
    val point = createPoint(data.x, data.y)
    if(bCheckMembership) assert(isMember(point))
    point
  }


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
    val exponentiations = exponentiationsMap.getOrElse(key = base, {
      // if there is no object that matches this base - create it and add it to the map
      val exps = new GroupElementsExponentiations(base)
      exponentiationsMap.put(base, exps)
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
  override def endExponentiateWithPreComputedValues(base: ElemType): Unit = exponentiationsMap -= base

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
    val h = if ((n % w) == 0) n / w else (n / w).toInt + 1

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
    BigIntegers.createRandomInRange(one, qMinusOne, random)
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
    BigIntegers.createRandomInRange(one, qMinusOne, random)
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