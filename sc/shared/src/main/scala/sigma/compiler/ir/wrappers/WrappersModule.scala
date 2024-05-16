package sigma.compiler.ir.wrappers

import sigma.compiler.ir.IRContext
import sigma.compiler.ir.wrappers.scala.WOptionsModule
import sigma.compiler.ir.wrappers.scalan.WRTypesModule
import sigma.compiler.ir.wrappers.special.WSpecialPredefsModule

trait WrappersModule
  extends WSpecialPredefsModule
  with WOptionsModule
  with WRTypesModule { self: IRContext => }