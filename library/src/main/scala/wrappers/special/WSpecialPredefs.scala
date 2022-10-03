package wrappers.special {
  import scalan._

  import special.wrappers.WrappersModule

  trait WSpecialPredefs extends Base { self: WrappersModule =>
    import WOption._;
    import WSpecialPredef._;
    @External("SpecialPredef") trait WSpecialPredef extends Def[WSpecialPredef];
    trait WSpecialPredefCompanion {
      @External def optionGetOrElse[A](opt: Ref[WOption[A]], default: Ref[A]): Ref[A];
      @External def none[A](implicit emA: Elem[A]): Ref[WOption[A]];
      @External def some[A](x: Ref[A]): Ref[WOption[A]];
      @External def cast[T](v: Ref[Any])(implicit emT: Elem[T]): Ref[WOption[T]];
      @External def loopUntil[A](s1: Ref[A], isMatch: Ref[scala.Function1[A, Boolean]], step: Ref[scala.Function1[A, A]]): Ref[A]
    }
  }
}