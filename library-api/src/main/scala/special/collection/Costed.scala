package special.collection

import scalan._

@WithMethodCallRecognizers
trait Costed[Val] {
  def builder: CostedBuilder
  def value: Val
  def cost: Int
  def size: Size[Val]
}

trait CostedPrim[Val] extends Costed[Val] {
  def value: Val
  def cost: Int
  def size: Size[Val]
}

trait CostedPair[L,R] extends Costed[(L,R)] {
  def l: Costed[L]
  def r: Costed[R]
  def accCost: Int
}

trait CostedFunc[Env,Arg,Res] extends Costed[Arg => Res]  {
  def envCosted: Costed[Env]
  def func: Costed[Arg] => Costed[Res]
  def cost: Int
  def sliceCalc: Arg => Res
  def sliceCost: ((Int,Size[Arg])) => Int
  def sliceCostEx: ((Arg, (Int,Size[Arg]))) => Int
  def sliceSize: Size[Arg] => Size[Res]
}

@WithMethodCallRecognizers
trait CostedColl[Item] extends Costed[Coll[Item]] {
  def values: Coll[Item]
  def costs: Coll[Int]
  def sizes: Coll[Size[Item]]
  def valuesCost: Int
  def mapCosted[Res](f: Costed[Item] => Costed[Res]): CostedColl[Res]
  def filterCosted(f: Costed[Item] => Costed[Boolean]): CostedColl[Item]
  def foldCosted[B](zero: Costed[B], op: Costed[(B, Item)] => Costed[B]): Costed[B]
}

/** NOTE: Option is a special case of Either, such that Option[T] is isomorphic to Either[Unit, T].
  * Keeping this in mind, we however define constructions for Option separately. */
trait CostedOption[T] extends Costed[Option[T]] {
  def costOpt: Option[Int]
  def sizeOpt: Option[Size[T]]
  def accumulatedCost: Int
}

@WithMethodCallRecognizers
trait CostedBuilder {
  def ConstructTupleCost: Int = 1
  def ConstructSumCost: Int = 1
  def SelectFieldCost: Int = 1
  def SumTagSize: Long = 1
  def costedValue[T](x: T, optCost: Option[Int])(implicit cT: RType[T]): Costed[T]
  def defaultValue[T](valueType: RType[T]): T
  def monoidBuilder: MonoidBuilder
  def mkSizePrim[T](dataSize: Long, tT: RType[T]): SizePrim[T]
  def mkSizePair[L,R](l: Size[L], r: Size[R]): SizePair[L,R]
  def mkSizeColl[T](sizes: Coll[Size[T]]): SizeColl[T]
  def mkSizeFunc[E,A,R](sizeEnv: Size[E], sizeFunc: Long, tA: RType[A], tR: RType[R]): SizeFunc[E,A,R]
  def mkSizeOption[T](sizeOpt: Option[Size[T]]): SizeOption[T]

  def mkCostedPrim[T](value: T, cost: Int, size: Size[T]): CostedPrim[T]
  def mkCostedPair[L,R](first: Costed[L], second: Costed[R], accCost: Int): CostedPair[L,R]
  def mkCostedFunc[Env,Arg,Res](envCosted: Costed[Env], func: Costed[Arg] => Costed[Res], cost: Int, size: Size[Arg=>Res]): CostedFunc[Env, Arg, Res]
  def mkCostedColl[T](values: Coll[T], costs: Coll[Int], sizes: Coll[Size[T]], valuesCost: Int): CostedColl[T]
  def mkCostedOption[T](value: Option[T], costOpt: Option[Int], sizeOpt: Option[Size[T]], accumulatedCost: Int): CostedOption[T]
}


