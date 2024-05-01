package scalan.meta

import sigma.util.StringUtil.StringUtilExtensions

case class ImportItem(packageName: String, importedNames: List[String])

case class SSymName(packageName: String, name: String) {
  import SSymName._
  def mkFullName = fullNameString(packageName, name)
}

object SSymName {
  /** Wildcard character used to signify importing all names from namespace */
  val ImportAllWildcard = "*"
  def fullNameString(packageName: String, name: String): String =
    if (packageName.isNullOrEmpty) name else s"$packageName.$name"
}