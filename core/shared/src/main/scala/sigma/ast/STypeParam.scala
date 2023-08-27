package sigma.ast

case class STypeParam(
    ident: STypeVar,
    upperBound: Option[SType] = None,
    lowerBound: Option[SType] = None) {
  assert(upperBound.isEmpty && lowerBound.isEmpty, s"Type parameters with bounds are not supported, but found $this")

  override def toString = ident.toString + upperBound.fold("")(u => s" <: $u") + lowerBound.fold("")(l => s" >: $l")
}

object STypeParam {
  implicit def typeIdentToTypeParam(id: STypeVar): STypeParam = STypeParam(id)
}