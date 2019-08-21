package special.collection

import scalan._
import special.SpecialPredef._

class CCostedPrim[Val](val value: Val, val cost: Int, val size: Size[Val]) extends CostedPrim[Val] {
  def builder: CostedBuilder = new CCostedBuilder
}

class CCostedPair[L,R](val l: Costed[L], val r: Costed[R], val accCost: Int) extends CostedPair[L,R] {
  def builder: CostedBuilder = new CCostedBuilder
  def value: (L,R) = (l.value, r.value)
  @NeverInline
  def cost: Int = rewritableMethod  // implementation is only expressible in staged code (see RW rule)
  def size: Size[(L,R)] = builder.mkSizePair(l.size, r.size)
}

/** @param cost Cost of creating the closure object of this function, doesn't include the cost of creating environment
  * @param dataSize Size of memory necessary to store the closure object, doesn't include the dataSize of storing environment
  * */
class CCostedFunc[Env,Arg,Res](
      val envCosted: Costed[Env],
      val func: Costed[Arg] => Costed[Res],
      val cost: Int,
      val size: Size[Arg => Res]) extends CostedFunc[Env, Arg, Res]
{
  def builder: CostedBuilder = new CCostedBuilder
  @NeverInline def value: Arg => Res = rewritableMethod
  override def sliceCalc: Arg => Res = rewritableMethod
  override def sliceCost: ((Int, Size[Arg])) => Int = rewritableMethod
  override def sliceCostEx: ((Arg, (Int, Size[Arg]))) => Int = rewritableMethod
  override def sliceSize: Size[Arg] => Size[Res] = rewritableMethod
}

class CCostedColl[Item](
      val values: Coll[Item],
      val costs: Coll[Int],
      val sizes: Coll[Size[Item]],
      val valuesCost: Int) extends CostedColl[Item]
{
  def builder: CostedBuilder = new CCostedBuilder
  def value: Coll[Item] = values
  @NeverInline
  def cost: Int = rewritableMethod // implementation is only expressible in staged code (see RW rule)
  def size: Size[Coll[Item]] = builder.mkSizeColl(sizes)
  @NeverInline
  def mapCosted[Res](f: Costed[Item] => Costed[Res]): CostedColl[Res] = rewritableMethod
  @NeverInline
  def filterCosted(f: Costed[Item] => Costed[Boolean]): CostedColl[Item] = rewritableMethod
  @NeverInline
  def foldCosted[B](zero: Costed[B], op: Costed[(B, Item)] => Costed[B]): Costed[B] = rewritableMethod
}

class CCostedBuilder extends CostedBuilder {
  @NeverInline
  def monoidBuilder: MonoidBuilder = new MonoidBuilderInst

  @NeverInline
  def costedValue[T](x: T, optCost: Option[Int])(implicit cT: RType[T]): Costed[T] = rewritableMethod

  @NeverInline
  def defaultValue[T](valueType: RType[T]): T = rewritableMethod

  def mkSizePrim[T](dataSize: Long, tT: RType[T]): SizePrim[T] =
    new CSizePrim[T](dataSize, tT)

  def mkSizePair[L, R](l: Size[L], r: Size[R]): SizePair[L, R] =
    new CSizePair(l, r)

  def mkSizeColl[T](sizes: Coll[Size[T]]): SizeColl[T] =
    new CSizeColl[T](sizes)

  def mkSizeFunc[E, A, R](sizeEnv: Size[E], sizeFunc: Long, tA: RType[A], tR: RType[R]): SizeFunc[E, A, R] =
    new CSizeFunc[E, A, R](sizeEnv, sizeFunc, tA, tR)

  def mkSizeOption[T](sizeOpt: Option[Size[T]]): SizeOption[T] =
    new CSizeOption[T](sizeOpt)

  def mkCostedPrim[T](value: T, cost: Int, size: Size[T]): CostedPrim[T] =
    new CCostedPrim[T](value, cost, size)

  def mkCostedPair[L,R](first: Costed[L], second: Costed[R], accCost: Int): CostedPair[L,R] =
    new CCostedPair(first, second, accCost)

  def mkCostedFunc[Env,Arg,Res](
        envCosted: Costed[Env],
        func: Costed[Arg] => Costed[Res],
        cost: Int, size: Size[Arg=>Res]): CostedFunc[Env, Arg, Res]
    = new CCostedFunc(envCosted, func, cost, size)

  def mkCostedColl[T](values: Coll[T], costs: Coll[Int], sizes: Coll[Size[T]], valuesCost: Int): CostedColl[T] =
    new CCostedColl[T](values, costs, sizes, valuesCost)

  def mkCostedOption[T](value: Option[T], costOpt: Option[Int], sizeOpt: Option[Size[T]], accumulatedCost: Int): CostedOption[T] =
    new CCostedOption[T](value, costOpt, sizeOpt, accumulatedCost)
}


