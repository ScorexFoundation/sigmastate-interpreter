package wrappers.scala {
  import scalan._

  import special.wrappers.WrappersModule

  trait WOptions extends Base { self: WrappersModule =>
    trait WOption[A] extends Def[WOption[A]] {
      implicit def eA: Elem[A];
      def isDefined: Ref[Boolean];
      def filter(p: Ref[scala.Function1[A, Boolean]]): Ref[WOption[A]];
      def map[B](f: Ref[scala.Function1[A, B]]): Ref[WOption[B]];
      def getOrElse[B](default: Ref[Thunk[B]]): Ref[B];
      def get: Ref[A]
    };
  }
}