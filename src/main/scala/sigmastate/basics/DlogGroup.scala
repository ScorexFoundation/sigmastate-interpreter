package sigmastate.basics

import java.math.BigInteger
import java.security.SecureRandom
import org.bouncycastle.math.ec.ECPoint


/**
  * This is the general interface for the discrete logarithm prime-order group.
  * Every class in the DlogGroup family implements this interface.
  *
  *
  * The discrete logarithm problem is as follows: given a generator g of a finite
  * group G and a random element h in G, find the (unique) integer x such that
  * `g^x = h`.
  *
  * In cryptography, we are interested in groups for which the discrete logarithm problem
  * (Dlog for short) is assumed to be hard. The most known groups of that kind are some Elliptic curve groups.
  *
  *  @tparam ElemType is concrete type
  */
trait DlogGroup[ElemType <: ECPoint] {

  val secureRandom = new SecureRandom()

  /**
    * The generator g of the group is an element of the group such that, when written multiplicatively, every element
    * of the group is a power of g.
    * @return the generator of this Dlog group
    */
  def generator: ElemType

  /**
    *
    * @return the order of this Dlog group
    */
  def order: BigInteger


  /**
    *
    * @return the identity element of this Dlog group
    */
  def identity: ElemType

  /**
    * Checks if the order of this group is greater than `2^numBits`
    * @param numBits
    * @return <code>true</code> if the order is greater than `2^numBits`;<p>
    * 		   <code>false</code> otherwise.
    */
  def orderGreaterThan(numBits: Int): Boolean

  /**
    * Calculates the inverse of the given GroupElement.
    * @param groupElement to invert
    * @return the inverse element of the given GroupElement
    * @throws IllegalArgumentException
    **/
  def inverseOf(groupElement: ElemType): ElemType

  /**
    * Raises the base GroupElement to the exponent. The result is another GroupElement.
    * @param exponent
    * @param base
    * @return the result of the exponentiation
    * @throws IllegalArgumentException
    */
  def exponentiate(base: ElemType, exponent: BigInteger): ElemType

  /**
    * Multiplies two GroupElements
    * @param groupElement1
    * @param groupElement2
    * @return the multiplication result
    * @throws IllegalArgumentException
    */
  def multiplyGroupElements(groupElement1: ElemType, groupElement2: ElemType): ElemType

  /**
    * Creates a random member of this Dlog group
    * @return the random element
    */
  def createRandomElement(): ElemType

  /**
    * Creates a random generator of this Dlog group
    *
    * @return the random generator
    */
  def createRandomGenerator(): ElemType = {
    // in prime order groups every element except the identity is a generator.
    // get a random element in the group
    var randGen = createRandomElement()

    // if the given element is the identity, get a new random element
    while ( {
      randGen.isInfinity
    }) randGen = createRandomElement()

    randGen
  }

  /**
    * Computes the product of several exponentiations with distinct bases
    * and distinct exponents.
    * Instead of computing each part separately, an optimization is used to
    * compute it simultaneously.
    * @param groupElements
    * @param exponentiations
    * @return the exponentiation result
    */
  def simultaneousMultipleExponentiations(groupElements: Array[ElemType], exponentiations: Array[BigInteger]): ElemType

  /**
    * Computes the product of several exponentiations of the same base
    * and distinct exponents.
    * An optimization is used to compute it more quickly by keeping in memory
    * the result of h1, h2, h4,h8,... and using it in the calculation.<p>
    * Note that if we want a one-time exponentiation of h it is preferable to use the basic exponentiation function
    * since there is no point to keep anything in memory if we have no intention to use it.
    * @param base
    * @param exponent
    * @return the exponentiation result
    */
  def exponentiateWithPreComputedValues(base: ElemType, exponent: BigInteger): ElemType

  /**
    * This function cleans up any resources used by exponentiateWithPreComputedValues for the requested base.
    * It is recommended to call it whenever an application does not need to continue calculating exponentiations for this specific base.
    *
    * @param base
    */
  def endExponentiateWithPreComputedValues(base: ElemType)

  /**
    * This function returns the value <I>k</I> which is the maximum length of a string to be encoded to a Group Element of this group.<p>
    * Any string of length <I>k</I> has a numeric value that is less than (p-1)/2 - 1.
    * <I>k</I> is the maximum length a binary string is allowed to be in order to encode the said binary string to a group element and vice-versa.<p>
    * If a string exceeds the <I>k</I> length it cannot be encoded.
    * @return k the maximum length of a string to be encoded to a Group Element of this group. k can be zero if there is no maximum.
    */
  def maxLengthOfByteArrayForEncoding: Int

}
