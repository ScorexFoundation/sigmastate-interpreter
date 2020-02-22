package sigmastate.utils

object GenPredefTypesApp extends SpecGen {
  import SpecGenUtils._

  def main(args: Array[String]) = {
    val table = printTypes(typeCompanions)
    saveFile("docs/spec/generated/predeftypes.tex", table)

    for (tc <- typesWithMethods) {
      val typeName = tc.typeName
      val methodsRows = printMethods(tc)
      saveFile(s"docs/spec/generated/${typeName}_methods.tex", methodsRows)

      println(s"\\input{generated/${typeName}_methods.tex}")
    }
  }
}
