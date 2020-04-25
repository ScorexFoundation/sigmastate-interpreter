package special.collection

import special.SpecialPredef
import special.SpecialPredef._
import scalan.{NeverInline, RType}

/** CostedOption is represented similar to CostedCol, because option can be represented as collection of 0 or 1 elements.
  * @param value optional value
  * @param costOpt optional cost of producing the optional value
  * @param sizeOpt optional size of the optional value
  * @param accumulatedCost accumulated cost to produce this option object (but not the cost of producing the value it may cost) */
class CCostedOption[T](
    val value: Option[T],
    val costOpt: Option[Int],
    val sizeOpt: Option[Size[T]],
    val accumulatedCost: Int
    ) extends CostedOption[T]
{
  def builder: CostedBuilder = new CCostedBuilder
  @NeverInline
  def cost: Int = rewritableMethod
  def size: Size[Option[T]] = builder.mkSizeOption(sizeOpt)
}

