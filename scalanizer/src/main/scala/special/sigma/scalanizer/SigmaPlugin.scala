package special.sigma.scalanizer

import spu.device.config.SigmaLibraryConfig

import scala.tools.nsc.Global
import scalan.meta.{ConfMap, TargetModuleConf, SourceModuleConf}
import scalan.meta.scalanizer.ScalanizerConfig
import scalan.plugin.{ScalanizerPluginConfig, ScalanizerPlugin}

class SigmaPlugin(g: Global) extends ScalanizerPlugin(g) { plugin =>
  override def createScalanizerConfig(): ScalanizerConfig = new SigmaScalanizerConfig
}

class SigmaScalanizerConfig extends ScalanizerPluginConfig {
  val sigma = new SigmaLibraryConfig()

  /** Modules that contain units to be virtualized by scalan-meta. */
  override val sourceModules: ConfMap[SourceModuleConf] = ConfMap(sigma.sourceModules: _*)
  override val targetModules: ConfMap[TargetModuleConf] = ConfMap(sigma.targetModules: _*)
}
