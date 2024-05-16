package sigma.compiler.ir.wrappers.scalan {
  import scalan._
  import sigma.compiler.ir.{Base, IRContext}


  trait WRTypes extends Base { self: IRContext =>
    trait WRType[A] extends Def[WRType[A]] {
      implicit def eA: Elem[A];
      def name: Ref[String]
    };
    trait WRTypeCompanion
  }
}