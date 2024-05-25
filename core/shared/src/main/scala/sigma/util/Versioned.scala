package sigma.util

import sigma.VersionContext

import scala.reflect.ClassTag

/** Represents a versioned object that can be created for each supported version.
  * The object is created lazily and cached for each version.
  *
  * @param builder a total function that creates an object for a given version in [0,
  *                maxVersion] range.
  * @param maxVersion the maximum supported version.
  */
case class Versioned[T <: AnyRef: ClassTag](builder: Byte => T, maxVersion: Byte = VersionContext.MaxSupportedScriptVersion) {
  private val cache = new Array[T](maxVersion + 1)

  def get(version: Byte): T = {
    require(version <= VersionContext.MaxSupportedScriptVersion, s"Not supported version $version")
    if (cache(version) == null) {
      val v = builder(version)
      cache(version) = v
      v
    } else {
      cache(version)
    }
  }
}
