package sigma

import sigma.data.CSigmaDslBuilder

package object eval {
  /** The primary reference to Global instance of SigmaDsl.
    * Besides operations of SigmaDslBuilder class, this instance also contains methods,
    * which are not available in Dsl code, and which are not in SigmaDslBuilder interface.
    * For example methods like `Box`, `toErgoBox` are available here, but not available in Dsl.
    *
    * @see SigmaDslBuilder
    */
  val SigmaDsl = CSigmaDslBuilder
}
