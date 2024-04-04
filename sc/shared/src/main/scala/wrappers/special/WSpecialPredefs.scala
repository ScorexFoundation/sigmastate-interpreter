package wrappers.special {
  import scalan._

  import special.wrappers.WrappersModule

  trait WSpecialPredefs extends Base { self: WrappersModule =>
    trait WSpecialPredef extends Def[WSpecialPredef];
    trait WSpecialPredefCompanion {
      def some[A](x: Ref[A]): Ref[WOption[A]];
    }
  }
}