package special.sigma.config

import special.library.config.SpecialLibraryConfig
import scalan.Liftable
import scalan.meta.ScalanAst.WrapperConf
import scalan.meta.{LibraryConfig, ConfMap, TargetModuleConf, SourceModuleConf}

class SigmaLibraryConfig extends LibraryConfig {
  def name = "sigma"
  def baseDir = ""
  val specialLibrary = new SpecialLibraryConfig

  def wrapperConfigs: Map[String, WrapperConf] = List(
    WrapperConf(baseDir,
      packageName = "special.sigma",
      name = "SigmaPredef"
    )
  ).map(w => (w.name, w)).toMap

  val ApiModule: SourceModuleConf = new SourceModuleConf(baseDir, "sigma-api")
      .moduleDependencies(specialLibrary.ApiModule)
      .addUnit("special/sigma/wrappers/WrappersSpec.scala", wrapperConfigs)
      .addUnit("special/sigma/SigmaDsl.scala")
      .addUnit("special/sigma/CostedObjects.scala")

  val ImplModule = new SourceModuleConf(baseDir, "sigma-impl")
      .moduleDependencies(specialLibrary.ApiModule, specialLibrary.ImplModule)
//      .addUnit("special/sigma/SigmaDslOverArrays.scala")
      .addUnit("special/sigma/SigmaDslCosted.scala")
      .dependsOn(ApiModule)

  val TargetModule = new TargetModuleConf(baseDir, "sigma-library",
    sourceModules = ConfMap()
        .add(ApiModule)
        .add(ImplModule))

  def sourceModules = List(ApiModule, ImplModule)
  def targetModules = List(TargetModule)
}
