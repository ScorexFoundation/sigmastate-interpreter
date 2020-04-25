package special.collection

import scalan.{RType, WithMethodCallRecognizers}

@scalan.Liftable
@WithMethodCallRecognizers
trait Size[Val] {
  def dataSize: Long
}

@scalan.Liftable
trait SizePrim[Val] extends Size[Val] {
  def dataSize: Long
  def tVal: RType[Val]
}

@scalan.Liftable
@WithMethodCallRecognizers
trait SizePair[L,R] extends Size[(L,R)] {
  def l: Size[L]
  def r: Size[R]
}

@scalan.Liftable
@WithMethodCallRecognizers
trait SizeColl[Item] extends Size[Coll[Item]] {
  def sizes: Coll[Size[Item]]
}

@scalan.Liftable
@WithMethodCallRecognizers
trait SizeFunc[Env, Arg, Res] extends Size[Arg => Res] {
  def sizeEnv: Size[Env]
}

@scalan.Liftable
@WithMethodCallRecognizers
trait SizeOption[T] extends Size[Option[T]] {
  def sizeOpt: Option[Size[T]]
}

