package sigma.types

import spire.util.Opt

object PrimType {
  def mkPrimValue[Val](value: Val): Opt[PrimValue[Val]] = (value match {
    case x: scala.Boolean => Opt(CBoolean(x))
    case x: scala.Byte    => Opt(CByte(x))
    case x: scala.Int     => Opt(CInt(x))
    case _ => Opt.empty[Val]
  }).asInstanceOf[Opt[PrimValue[Val]]]
}
