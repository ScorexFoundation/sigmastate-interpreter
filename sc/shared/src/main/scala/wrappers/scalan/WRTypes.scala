package wrappers.scalan {
  import scalan._
  import special.wrappers.WrappersModule


  trait WRTypes extends Base { self: WrappersModule =>
    trait WRType[A] extends Def[WRType[A]] {
      implicit def eA: Elem[A];
      def name: Ref[String]
    };
    trait WRTypeCompanion
  }
}