package general.automagic;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

import general.Replication;
import general.SystemState;
import general.annotations.Initialize;
import general.annotations.StopCriterium;

/**
 * This class provides a Replication that automatically detects which
 * methods in a certain subclass of SystemState serve as initialization
 * methods, and which ones serve as termination criteria, based on 
 * annotations. This circumvents the need to write your own subclass of
 * Replication
 * 
 * @author Paul Bouman
 *
 * @param <S> the type of the State in this simulation
 */

public class AutoReplication<S extends SystemState<S>> extends Replication<S> {

	private List<Method> initMethods;
	private List<Method> terminateMethodsWithArg;
	private List<Method> terminateMethods;
	
	public AutoReplication(S state) {
		super(state);
		
		this.terminateMethods = new ArrayList<>();
		this.terminateMethodsWithArg = new ArrayList<>();
		this.initMethods = new ArrayList<>();
		for (Method m : state.getClass().getMethods()) {
			if (m.isAnnotationPresent(Initialize.class)) {
				if (m.getParameterCount() != 0) {
					throw new IllegalArgumentException("Method "+m+" has an "
							+ "@Initialize annotation, but has one or more arguments.");
				}
				this.initMethods.add(m);
			}
			if (m.isAnnotationPresent(StopCriterium.class)) {
				if (m.getParameterCount() > 1) {
					throw new IllegalArgumentException("Method "+m+" has a "
							+ "@ShouldTerminate annotation, but has one or more arguments.");
				}
				if (!isBoolean(m.getReturnType())) {
					throw new IllegalArgumentException("Method "+m+" has a "
							+ "@ShouldTerminate annotation, but the return type is not Boolean.");
				}
				if (m.getParameterCount() == 1) {
					this.terminateMethodsWithArg.add(m);
				}
				else {
					this.terminateMethods.add(m);
				}
			}
		}
		
		if (initMethods.isEmpty()) {
			throw new IllegalArgumentException("The provided state must have at least one method "
					+ "with the @Initialize annotation that is used to initialize the state and "
					+ "insert starting events at the beginning of a simulation run");
		}
	}
	
	public static boolean isBoolean(Class<?> cls) {
		return Boolean.class.isAssignableFrom(cls)
			|| boolean.class.isAssignableFrom(cls);
	}

	@Override
	public void initialize() {
		S state = getSystemState();
		for (Method m : initMethods) {
			try {
				m.invoke(state);
			} catch (IllegalAccessException | IllegalArgumentException ex) {
				throw new RuntimeException("Unexpected exception while executing replication initializers", ex);
			} catch (InvocationTargetException ex) {
				throw new RuntimeException("While running init method "+m+", an exception occured", ex);
			}
			
		}
	}

	@Override
	public boolean shouldTerminate(S state) {
		
			try {
				for (Method m : terminateMethods) {
					Boolean b = (Boolean) m.invoke(state);
					if (b) {
						return true;
					}
				}
				for (Method m : terminateMethodsWithArg) {
					Boolean b = (Boolean) m.invoke(state,state);
					if (b) {
						return true;
					}
				}
			} catch (IllegalAccessException | IllegalArgumentException ex) {
				throw new RuntimeException("Unexpected exception while checking for termination", ex);
			} catch (InvocationTargetException ex) {
				throw new RuntimeException("While running a termination method an exception occured", ex.getCause());
			}
		return false;
	}

}
