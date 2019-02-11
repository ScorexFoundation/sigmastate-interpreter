package special.sigma.wrappers

import wrappers.java.math.WBigIntegersModule
import wrappers.org.bouncycastle.math.ec.WECPointsModule
import wrappers.special.sigma.WSigmaPredefsModule

trait WrappersModule
  extends special.wrappers.WrappersModule
     with WSigmaPredefsModule
     with WECPointsModule
     with WBigIntegersModule
