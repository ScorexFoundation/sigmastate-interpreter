package scalan

import scala.annotation.unused

object Platform {
  /** In JS tests do nothing. (The corresponding JVM method outputs graphs into files. */
  def stage[Ctx <: Scalan](scalan: Ctx)(
      @unused prefix: String,
      @unused testName: String,
      @unused name: String,
      @unused sfs: Seq[() => scalan.Sym]): Unit = {
  }
}
