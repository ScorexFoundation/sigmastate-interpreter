package scalan;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** Used in generated wrappers.
 * Annotates a wrapper method of the companion of a virtualized type wrapper,
 * which (the method) corresponds to the constructor of wrapped type. */
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.RUNTIME)
public @interface Constructor {
}
