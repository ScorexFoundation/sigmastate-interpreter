package scalan;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** Specifies the predicate when the annotated method should be inlined.
* The predicate is given by the parsable formula which can be used to
* create runtime predicate functions.
* Default empty string is interpreted as never invoke, in which case scalanizer
* don't perform virtualization of the method body and replace it with delayInvoke. */
@Target({ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface InlineAt {
    String Never = "";
    String value() default Never;
}
