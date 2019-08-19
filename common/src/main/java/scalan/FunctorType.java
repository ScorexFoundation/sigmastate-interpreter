package scalan;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** Used to annotate container types (like Option, Coll), which have
 * functor semantics. Special code is generated for such entities. */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.CLASS)
public @interface FunctorType {
}
