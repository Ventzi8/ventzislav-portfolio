package general.annotations;

import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

/**
 * This annotation is used to indicate that a method in a SystemState subclass
 * can be used by the {@link general.automagic.AutoReplication} to initialize a replication.
 * Annotated methods are not supposed to have any arguments. If multiple annotated
 * methods occur within the same SystemState subclass, the order in which they are
 * executed is not defined.
 * 
 * @author Paul Bouman
 *
 */
@Retention(RUNTIME)
@Target(METHOD)
public @interface Initialize {

}
