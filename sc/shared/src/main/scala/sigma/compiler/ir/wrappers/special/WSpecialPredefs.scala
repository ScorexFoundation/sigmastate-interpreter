package sigma.compiler.ir.wrappers.special {
  import scalan._
  import sigma.compiler.ir.{Base, IRContext}

  trait WSpecialPredefs extends Base { self: IRContext =>
    trait WSpecialPredef extends Def[WSpecialPredef];
    trait WSpecialPredefCompanion {
      def some[A](x: Ref[A]): Ref[WOption[A]];
    }
  }
}