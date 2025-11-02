package general.annotations;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

/**
 * Annotation that is used to signal that a field within a SystemState subclass
 * should be initialized as a Counter that is managed by the Replication and
 * Simulation classes of the simulation framework.
 * 
 * This annotation is meant for fields of type Count only. Annotated fields
 * need not be initialized, as long as the constructor of the SystemState
 * superclass is executed.
 * 
 * @author Paul Bouman
 *
 */

@Retention(RUNTIME)
@Target(FIELD)
public @interface AutoCounter {
	/**
	 * @return The description of this Counter
	 */
	String value();
	/**
	 * @return The initial value of this counter after the simulation is reset.
	 * The default value is 0.
	 */
	double initialValue() default 0d;
}
