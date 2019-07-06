package special.sigma.wrappers {
  import scalan._

  trait WrappersSpec extends Base { self: SigmaLibrary =>
    import WSigmaPredef._;
    import WrapSpecBase._;
    trait SigmaPredefWrapSpec extends WrapSpecBase {
      def dataSize(v: Rep[Any]): Rep[Long] = RWSigmaPredef.dataSize[Any](v)
    };
    trait SigmaPredefWrapSpecCompanion
  }
}