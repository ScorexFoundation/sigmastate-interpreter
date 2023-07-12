package wrappers.scalan {
  import scalan._

  import impl._

  import scalan.RType

  import special.wrappers.WrappersModule

  import special.wrappers.RTypeWrapSpec

  import scala.collection.mutable.WrappedArray

  trait WRTypes extends Base { self: WrappersModule =>
    import WRType._;
    trait WRType[A] extends Def[WRType[A]] {
      implicit def eA: Elem[A];
      def name: Ref[String]
    };
    trait WRTypeCompanion
  }
}