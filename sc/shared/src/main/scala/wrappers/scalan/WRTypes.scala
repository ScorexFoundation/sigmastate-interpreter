package wrappers.scalan {
  import scalan._
  import sigma.compiler.{Base, Scalan}


  trait WRTypes extends Base { self: Scalan =>
    trait WRType[A] extends Def[WRType[A]] {
      implicit def eA: Elem[A];
      def name: Ref[String]
    };
    trait WRTypeCompanion
  }
}