package wrappers.special {
  import scalan._
  import sigma.compiler.{Base, Scalan}

  trait WSpecialPredefs extends Base { self: Scalan =>
    trait WSpecialPredef extends Def[WSpecialPredef];
    trait WSpecialPredefCompanion {
      def some[A](x: Ref[A]): Ref[WOption[A]];
    }
  }
}