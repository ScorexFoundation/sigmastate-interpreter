package scalan;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;


/** Relate annotated element with internal type or method given by `value` property.
* Applied to entities that should be replaced during virtualization by related type.
* The given related type is said to be pre-virtualized.
* The 'value' is a name of the class which can be resolved in a Scalan cake. E.g. IsoUR*/
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface Builtin {
    String value() default "";  // default value interpreted as "virtualized name is equal to annotated element name"
}
