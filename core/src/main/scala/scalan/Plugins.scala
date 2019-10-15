package scalan

import java.io.File
import com.typesafe.config.ConfigFactory
import scalan.util.ClassLoaderUtil

object Plugins {
  val extraClassPathKey = "plugins.extraClassPath"

  lazy val config0 = ConfigFactory.load().getConfig("scalan")
  lazy val pluginClassLoader = {
    val thisClassLoader = getClass.getClassLoader

    config0.getString(extraClassPathKey) match {
      case "" =>
        thisClassLoader
      case path =>
        val files = path.split(File.pathSeparatorChar).map(new File(_))
        ClassLoaderUtil.URLClassLoader(files, thisClassLoader)
    }
  }
  lazy val configWithPlugins = ConfigFactory.load(pluginClassLoader).getConfig("scalan")

  def loadClass(name: String) = pluginClassLoader.loadClass(name)
}
