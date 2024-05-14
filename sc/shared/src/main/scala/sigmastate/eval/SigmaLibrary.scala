package sigmastate.eval

import scalan.Scalan
import sigma.SigmaDslModule

/** Main trait which represents sigma operations in graph IR cake. */
trait SigmaLibrary extends Scalan
    with sigma.wrappers.WrappersModule
    with SigmaDslModule
{
  import WRType._

  implicit lazy val wRTypeAnyElement: Elem[WRType[Any]] = wRTypeElement(AnyElement)

  /** During compilation represent a global value Global, see also SGlobal type. */
  def sigmaDslBuilder: Ref[SigmaDslBuilder]
}
