package general.annotations;

import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

/**
 * Annotation that is used to signal that certain methods can be used
 * to obtain performance measures from a SimulationState subclass. This
 * is used to automatically build PerformanceMeasure objects that are
 * managed by the Simulation and Replication classes of the simulation
 * framework.
 * 
 * Note that this annotation should only be applied to methods that
 * accept no arguments and have either a primitive numeric value, i.e.
 * int, double, long, float, short or byte, or a subclass of {@link Number}
 * as a return type.
 * 
 * @author Paul Bouman
 *
 */

@Retention(RUNTIME)
@Target(METHOD)
public @interface AutoMeasure {
	/**
	 * @return A description of the measure the annotation method 
	 */
	String value();
}
