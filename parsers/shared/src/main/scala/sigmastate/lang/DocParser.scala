package sigmastate.lang

case class DocToken(kind: DocToken.Kind, name: Option[String], body: Option[String]) {

}

object DocToken {
  /**
   * Represents a documentation remark.
   */
  sealed abstract class Kind

  /**
   * Represents a labeled documentation remark.
   */
  sealed abstract class TagKind(val label: String, val numberParameters: Int) extends Kind

  /**
   *
   * implement different tags like @param, @returns, etc
   *
   */
}

object DocParser {

}
