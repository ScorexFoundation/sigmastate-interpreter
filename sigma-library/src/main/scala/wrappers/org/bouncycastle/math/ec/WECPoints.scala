package wrappers.org.bouncycastle.math.ec {
  import scalan._

  import impl._

  import special.sigma.wrappers.WrappersModule

  import special.sigma.wrappers.ECPointWrapSpec

  trait WECPoints extends Base { self: WrappersModule =>
    import WArray._;
    import WBigInteger._;
    import WECPoint._;
    @External("ECPoint") @Liftable trait WECPoint extends Def[WECPoint] {
      @External def add(x$1: Rep[WECPoint]): Rep[WECPoint];
      @External def multiply(x$1: Rep[WBigInteger]): Rep[WECPoint];
      //todo remove compressed flag, use GroupElementSerializer
      @External def getEncoded(x$1: Rep[Boolean]): Rep[WArray[Byte]]
    };
    trait WECPointCompanion
  }
}