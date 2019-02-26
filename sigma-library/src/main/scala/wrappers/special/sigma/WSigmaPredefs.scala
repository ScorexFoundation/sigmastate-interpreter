package wrappers.special.sigma {
  import scalan._

  import impl._

  import special.sigma.wrappers.WrappersModule

  import special.sigma.wrappers.SigmaPredefWrapSpec

  trait WSigmaPredefs extends Base { self: WrappersModule =>
    import WSigmaPredef._;
    @External("SigmaPredef") trait WSigmaPredef extends Def[WSigmaPredef];
    trait WSigmaPredefCompanion {
      @External def dataSize[T](v: Rep[T]): Rep[Long]
    }
  }
}