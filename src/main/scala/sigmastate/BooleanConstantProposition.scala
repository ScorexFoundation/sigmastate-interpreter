package sigmastate


sealed trait BooleanConstantProposition extends SigmaStateProposition

object BooleanConstantProposition {
  def fromBoolean(b: Boolean) = b match {
    case true => TrueProposition
    case false => FalseProposition
  }
}


case object TrueProposition extends BooleanConstantProposition {
  override lazy val bytes: Array[Byte] = ???
}

case object FalseProposition extends BooleanConstantProposition {
  override lazy val bytes: Array[Byte] = ???
}