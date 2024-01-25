package scalan

import scalan.compilation.GraphVizExport
import sigma.util.FileUtil
import org.scalatest.Assertions

object Platform {
  /** Output graph given by symbols in `sfs` to files.
    *
    * @param scalan   The Scalan context
    * @param prefix   The prefix of the directory where the graphs will be stored.
    * @param testName The name of the test.
    * @param name     The name of the graph.
    * @param sfs      A sequence of functions that return symbols of the graph roots.
    */
  def stage[Ctx <: Scalan](scalan: Ctx)
                          (prefix: String, testName: String, name: String, sfs: Seq[() => scalan.Sym]): Unit = {
    val directory = FileUtil.file(prefix, testName)
    val gv = new GraphVizExport(scalan)
    implicit val graphVizConfig = gv.defaultGraphVizConfig
    try {
      val ss = sfs.map(_.apply()).asInstanceOf[Seq[gv.scalan.Sym]]
      gv.emitDepGraph(ss, directory, name)(graphVizConfig)
    } catch {
      case e: Exception =>
        val graphMsg = gv.emitExceptionGraph(e, directory, name) match {
          case Some(graphFile) =>
            s"See ${graphFile.file.getAbsolutePath} for exception graph."
          case None =>
            s"No exception graph produced."
        }
        Assertions.fail(s"Staging $name failed. $graphMsg", e)
    }
  }

  /** On JVM it calls Thread.sleep. */
  def threadSleepOrNoOp(millis: Long): Unit =
    Thread.sleep(millis)
}
