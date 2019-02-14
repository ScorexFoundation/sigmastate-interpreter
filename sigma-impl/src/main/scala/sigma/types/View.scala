package sigma.types

import spire.util.Opt

object View {
  def mkPrimView[Val](value: Val): Opt[PrimView[Val]] = (value match {
    case x: scala.Boolean => Opt(CBoolean(x))
    case x: scala.Byte    => Opt(CByte(x))
    case x: scala.Int     => Opt(CInt(x))
    case _ => Opt.empty[Val]
  }).asInstanceOf[Opt[PrimView[Val]]]
}
