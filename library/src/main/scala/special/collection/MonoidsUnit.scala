package special.collection {
  import scalan._

  trait Monoids extends Base { self: Library =>
    import Monoid._;
    import MonoidBuilder._;
    trait Monoid[T] extends Def[Monoid[T]] {
      implicit def eT: Elem[T];
      def zero: Ref[T];
      def plus(x: Ref[T], y: Ref[T]): Ref[T];
      def power(x: Ref[T], n: Ref[Int]): Ref[T]
    };
    @WithMethodCallRecognizers trait MonoidBuilder extends Def[MonoidBuilder] {
      def intPlusMonoid: Ref[Monoid[Int]];
      def longPlusMonoid: Ref[Monoid[Long]];
    };
    trait MonoidCompanion;
    trait MonoidBuilderCompanion
  }
}