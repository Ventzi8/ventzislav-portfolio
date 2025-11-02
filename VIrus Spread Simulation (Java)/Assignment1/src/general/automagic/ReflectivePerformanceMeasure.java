package general.automagic;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import general.PerformanceMeasure;
import general.SystemState;

/**
 * This class models performance measures that are computed by
 * calling a particular method in the state, by means of reflection.
 * 
 * The objects of this type use reflection to call a particular 
 * method of an object. This allows the SystemState class to
 * generate PerformanceMeasure objects from annotated methods
 * rather than to force the programmer to create explicit
 * subclasses of PerformanceMeasure.
 * 
 * @author Paul Bouman
 *
 * @param <S> the typ e of the state in the simulation
 */
public class ReflectivePerformanceMeasure<S extends SystemState<S>> extends PerformanceMeasure<S>
{
	private Method m;
	
	public ReflectivePerformanceMeasure(String name, Method m)
	{
		super(name);
		if (Modifier.isStatic(m.getModifiers())) {
			throw new IllegalArgumentException("Static methods are not supported for automated measuring");
		}
		if (!isNumericType(m.getReturnType())) {
			throw new IllegalArgumentException("Only methods that return a primitive numeric value or a"
					+ "subclass of Number, such as Integer or Double, can be measured automatically");
		}
		this.m = m;
	}

	public static boolean isNumericType(Class<?> cls) {
		if (Number.class.isAssignableFrom(cls)) {
			return true;
		}
		return cls.isPrimitive() && (
				   int.class.isAssignableFrom(cls)
			    || double.class.isAssignableFrom(cls)
			    || long.class.isAssignableFrom(cls)
			    || float.class.isAssignableFrom(cls)
			    || short.class.isAssignableFrom(cls)
			    || byte.class.isAssignableFrom(cls));
	}
	
	@Override
	public double compute(S state)
	{
		try {
			Object o = m.invoke(state);
			if (o == null) {
				return 0d;
			}
			else {
				Number n = (Number) o;
				return n.doubleValue();
			}
		} catch (IllegalAccessException | IllegalArgumentException e) {
			throw new RuntimeException("This is unexpected", e);
		} catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof RuntimeException) {
				throw (RuntimeException) cause;
			}
			else if (cause instanceof Error) {
				throw (Error) cause;
			}
			else {
				throw new RuntimeException("An error occurred while calling an automated performance measure", cause);
			}
		}
	}
	
}
