package special.collection

import scalan._

class CSizePrim[Val](val dataSize: Long, val tVal: RType[Val]) extends SizePrim[Val] {
}

class CSizePair[L,R](val l: Size[L], val r: Size[R]) extends SizePair[L, R] {
  @NeverInline
  def dataSize: Long = l.dataSize + r.dataSize
}

class CSizeColl[Item](val sizes: Coll[Size[Item]]) extends SizeColl[Item] {
  @Internal
  def builder: CostedBuilder = new CCostedBuilder

  @NeverInline
  def dataSize: Long = sizes.map(_.dataSize).sum(builder.monoidBuilder.longPlusMonoid)
}

class CSizeFunc[Env, Arg, Res](val sizeEnv: Size[Env], val sizeFunc: Long, val tArg: RType[Arg], val tRes: RType[Res]) extends SizeFunc[Env, Arg, Res] {
  @NeverInline
  def dataSize: Long = sizeEnv.dataSize + sizeFunc
}

class CSizeOption[Item](val sizeOpt: Option[Size[Item]]) extends SizeOption[Item] {
  @NeverInline
  def dataSize: Long = sizeOpt.map(_.dataSize).getOrElse(0L)
}
