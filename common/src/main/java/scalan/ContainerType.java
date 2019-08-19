package scalan;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/** Annotates Special DSL types which implement the interface of containers.
* Each container is described using Cont[_] descriptors.
* Special supporting code is generated for annotated entities. */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.CLASS)
public @interface ContainerType {
}
