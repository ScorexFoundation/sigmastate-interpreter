package sigma.ast.js

import sigma.ast.syntax.SValue
import sigma.js.JsWrapper

import scala.scalajs.js.annotation.JSExportTopLevel

/** An exported JavaScript class wrapping the Scala [[sigma.ast.Value]]. */
@JSExportTopLevel("Expr")
class Expr(override val wrappedValue: SValue) extends JsWrapper[SValue]{
}

@JSExportTopLevel("ExprObj")
object Expr {
}
