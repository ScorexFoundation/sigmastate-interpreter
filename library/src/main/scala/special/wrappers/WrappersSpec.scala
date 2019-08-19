package special.wrappers {
  import scalan._

  trait WrappersSpec extends Base { self: Library =>
    import WOption._;
    import WRType._;
    import WSpecialPredef._;
    import WrapSpecBase._;
    trait WrapSpecBase extends Def[WrapSpecBase] with WrapSpec;
    trait OptionWrapSpec extends WrapSpecBase {
      def get[A](xs: Ref[WOption[A]]): Ref[A] = xs.get;
      @NeverInline def getOrElse[A](xs: Ref[WOption[A]], default: Ref[Thunk[A]]): Ref[A] = delayInvoke;
      def map[A, B](xs: Ref[WOption[A]], f: Ref[scala.Function1[A, B]]): Ref[WOption[B]] = xs.map[B](f);
      def flatMap[A, B](xs: Ref[WOption[A]], f: Ref[scala.Function1[A, WOption[B]]]): Ref[WOption[B]] = xs.flatMap[B](f);
      def filter[A](xs: Ref[WOption[A]], f: Ref[scala.Function1[A, Boolean]]): Ref[WOption[A]] = xs.filter(f);
      def isDefined[A](xs: Ref[WOption[A]]): Ref[Boolean] = xs.isDefined;
      def isEmpty[A](xs: Ref[WOption[A]]): Ref[Boolean] = xs.isEmpty;
      @NeverInline def fold[A, B](xs: Ref[WOption[A]], ifEmpty: Ref[Thunk[B]], f: Ref[scala.Function1[A, B]]): Ref[B] = delayInvoke
    };
    trait SpecialPredefWrapSpec extends WrapSpecBase {
      def loopUntil[A](s1: Ref[A], isMatch: Ref[scala.Function1[A, Boolean]], step: Ref[scala.Function1[A, A]]): Ref[A] = RWSpecialPredef.loopUntil[A](s1, isMatch, step);
      def cast[A](v: Ref[Any])(implicit cA: Elem[A]): Ref[WOption[A]] = RWSpecialPredef.cast[A](v);
      def some[A](x: Ref[A]): Ref[WOption[A]] = RWSpecialPredef.some[A](x);
      def none[A](implicit cA: Elem[A]): Ref[WOption[A]] = RWSpecialPredef.none[A];
      def optionGetOrElse[A](opt: Ref[WOption[A]], default: Ref[A]): Ref[A] = RWSpecialPredef.optionGetOrElse[A](opt, default)
    };
    trait RTypeWrapSpec extends WrapSpecBase {
      def name[T](d: Ref[WRType[T]]): Ref[String] = d.name
    };
    trait WrapSpecBaseCompanion;
    trait OptionWrapSpecCompanion;
    trait SpecialPredefWrapSpecCompanion;
    trait RTypeWrapSpecCompanion
  }
}