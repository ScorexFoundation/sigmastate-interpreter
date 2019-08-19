package scalan;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** Type argument T should be reified in virtualized code type descriptor Elem[T] available
* in the scope where T is visible. This can be done either by explicitly passing additional
* parameter eT: Elem[T] or by proving that Elem[T] can be retrieved from the other arguments
* of the method or class. For example if x: Rep[T] then eT can be obtained by x.elem.
* The need for reified type arguments come from ClassTag[T], RType[T] context bounds or
* implicit parameters in the source code.
*/
@Target({ElementType.TYPE, ElementType.TYPE_PARAMETER})
@Retention(RetentionPolicy.RUNTIME)
public @interface Reified {
    String value() default "";
}
