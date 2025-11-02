package general.annotations;

import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

/**
 * This annotation is used to indicate that a method in a SystemState subclass
 * can be used by the {@link general.automagic.AutoReplication} to check whether the simulation
 * should terminate in the given state.
 * Annotated methods are not supposed to have any arguments and should have
 * <code>boolean</code> or @{link Boolean} as a return type. If multiple annotated
 * methods, the simulation stops if any of the methods returns true.
 * 
 * @author Paul Bouman
 *
 */
@Retention(RUNTIME)
@Target(METHOD)
public @interface StopCriterium {

}
