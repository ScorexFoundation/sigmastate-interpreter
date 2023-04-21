package scalan.meta

import scalan.util.StringUtil._

case class ImportItem(packageName: String, importedNames: List[String])

case class SSymName(packageName: String, name: String) {
  import SSymName._
  def this(name: String) = this("", name)
  def mkFullName = fullNameString(packageName, name)
  def isImportedBy(item: ImportItem): Boolean = {
    if (packageName != item.packageName) return false
    item.importedNames.contains(SSymName.ImportAllWildcard) || item.importedNames.contains(name)
  }
}

object SSymName {
  /** Wildcard character used to signify imporing all names from namespace */
  val ImportAllWildcard = "*"
  def fullNameString(packageName: String, name: String): String =
    if (packageName.isNullOrEmpty) name else s"$packageName.$name"
}