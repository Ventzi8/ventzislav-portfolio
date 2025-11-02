package general;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.PriorityQueue;
import java.util.Random;
import java.util.function.Consumer;

import general.annotations.AutoCounter;
import general.annotations.AutoMeasure;
import general.automagic.ConsumerEvent;
import general.automagic.ReflectivePerformanceMeasure;
import general.automagic.RunnableEvent;

/**
 * Basic skeleton for a system state. Already contains the simulation clock, the statistical counters, and an event queue. 
 * 
 * @author Nemanja Milovanovic
 *
 * @param <S> the state of a subclass, so the methods of this state can expose only the subclass type
 */

public abstract class SystemState<S extends SystemState<S>> {

	private double currentTime;
	private double timeHorizon;
	private final PriorityQueue<Event<S>> queue;
	
	private final List<Counter> counters;
	private final Random random;

	private Map<Field,Counter> autoCounters;
	
	public SystemState(double timeHorizon, long seed) {
		this(timeHorizon, null, seed);
	}
	
	public SystemState(
			double timeHorizon, 
			List<Counter> counters, 
			long seed) {
		currentTime = 0;
		random = new Random(seed);
		queue = new PriorityQueue<>();
		
		this.timeHorizon = timeHorizon;
		if (counters == null) {
			this.counters = getAutoCounters();
		}
		else {
			this.counters = counters;
		}
	}
	
	/**
	 * Updates the simulation clock.
	 * 
	 * @param newTime	New simulation clock time
	 */
	public void updateCurrentTime(double newTime) {
		currentTime = newTime;
	}
	
	/**
	 * 
	 * @return	The current simulation clock time
	 */
	public double getCurrentTime() {
		return currentTime;
	}
	
	/**
	 * 
	 * @return Returns the event queue
	 */
	public PriorityQueue<Event<S>> getQueue() {
		return queue;
	}
	
	/**
	 * Adds an {@link Event} to the event queue.
	 * 
	 * @param e	Event to be added to queue
	 */
	public void addEvent(Event<S> e) {
		if (e == null) {
			throw new IllegalArgumentException("Event cannot be null.");
		}
		// TODO: is dit ook geen goed idee, overigens??
		if (e.getTime() < getCurrentTime()) {
			throw new IllegalArgumentException("The simulation can not travel back in time.");
		}
		queue.add(e);
	}
	
	/**
	 * Convenience method that automatically creates an Event object based on a time
	 * the Event should be executed, and an action that accepts the time of the event
	 * as an input.
	 * @param time the time at which the event takes place
	 * @param action the action to perform at this time
	 */
	public void addEvent(double time, Consumer<Double> action) {
		Event<S> event = new ConsumerEvent<S>(time, action);
		addEvent(event);
	}
	
	/**
	 * Convenience method that automatically creates an Event object based on a time
	 * the Event should be executed, and an action that should be performed at that
	 * time.
	 * @param time the time at which the event should be executed
	 * @param action the action to execute
	 */
	public void addEvent(double time, Runnable action) {
		Event<S> event = new RunnableEvent<S>(time, action);
		addEvent(event);
	}
	
	/**
	 * @param e	Current event to be processed
	 * @return	True if <code>Event e</code> passed the simulation horizon, false otherwise
	 */
	public boolean passedTimeHorizon(Event<S> e) {
		double eventTime = e.getTime();
		return eventTime > timeHorizon;
	}
	
	/**
	 * The counters appear in the list in the same order they were passed to the constructor.
	 * In case the counters were generated automatically based on {@link AutoCounter} annotations,
	 * the order of this list is not defined.
	 * @return Returns a list of counters used in this replication
	 */
	public List<Counter> getCounters() {
		return new ArrayList<>(counters);
	}
	
	/**
	 * 
	 * @return Returns the {@link Random} object used in this DES
	 */
	public Random getRandom() {
		return random;
	}
	
	/**
	 * 
	 * @return Returns the simulation time horizon
	 */
	public double getTimeHorizon() {
		return timeHorizon;
	}
	
	/**
	 * Resets/initializes the system state. Note that only the event queue and user-specified features should be reset, as the simulation clock and counters already get reset automatically.
	 * <b>Note: Do not reset your random number generator.</b> Otherwise {@link Simulation} will use the same random numbers in each replication. 
	 */
	public abstract void reset();
	
	/**
	 * This analyzes the type of the current instance, and looks for methods with an @AutoMeasure annotation.
	 * These are then automatically wrapped within PerformanceMeasure objects. The result is a list of these
	 * automatically generated PerformanceMeasure objects.
	 * @return a list of automatically generated PerformanceMeasures based on the annotated method in the current type.
	 */
	public List<PerformanceMeasure<S>> getAutomaticMeasures() {
		List<PerformanceMeasure<S>> result = new ArrayList<>();
		Class<?> cls = this.getClass();
		for (Method m : cls.getMethods()) {
			if (m.isAnnotationPresent(AutoMeasure.class)) {
				if (m.getParameterCount() != 0) {
					throw new IllegalStateException("Method "+m+" has arguments, but this is not allowed for @AutoMeasure methods");
				}
				if (!ReflectivePerformanceMeasure.isNumericType(m.getReturnType())) {
					throw new IllegalStateException("Method "+m+" does not return a primitive numeric value or a subclass of Number, but this is required for @AutoMeasure methods");
				}
				String name = m.getAnnotation(AutoMeasure.class).value();
				ReflectivePerformanceMeasure<S> rpm = new ReflectivePerformanceMeasure<>(name, m);
				result.add(rpm);
			}
		}
		return result;
	}
	
	public void injectCounters() {
		if (autoCounters == null) {
			// Apparently, the counters are not managed automatically.
			return;
		}
		for (Entry<Field,Counter> e : autoCounters.entrySet()) {
			try {
				Field f = e.getKey();
				Counter c = e.getValue();
				boolean acc = f.isAccessible();
				f.setAccessible(true);
				Object cur = f.get(this);
				if (cur != null && cur != c) {
					throw new RuntimeException("It seems that @AutoCounter annotated field '"+f.getName()+"' in "+f.getDeclaringClass().getName()+" was assigned a new unmanaged value. "
							+ "Please make sure you never assign a value to an automatically managed Counter yourself.");
				}
				f.set(this, c);
				f.setAccessible(acc);
				autoCounters.put(f, c);
			} catch (IllegalArgumentException | IllegalAccessException ex) {
				throw new RuntimeException("Unexpected error while injecting @AutoCounter fields.", ex);
			}
		}
	}
	
	private List<Counter> getAutoCounters() {
		if (autoCounters == null) {
			autoCounters = new LinkedHashMap<>();
			Class<?> clz = this.getClass();
			for (Field f : clz.getDeclaredFields()) {
				if (f.isAnnotationPresent(AutoCounter.class)) {
					if (!Counter.class.isAssignableFrom(f.getType())) {
						throw new IllegalStateException("Field "+f+" has an @AutoCounter annotation but is not of type Counter.");
					}
					if (Modifier.isFinal(f.getModifiers())) {
						throw new IllegalStateException("Field "+f+" has an @AutoCounter annotation but is also final.");
					}
					AutoCounter ac = f.getAnnotation(AutoCounter.class);
					Counter c = new Counter(ac.initialValue(), ac.value());
					autoCounters.put(f, c);
				}
			}
		}
		injectCounters();
		return new ArrayList<>(autoCounters.values());
	}
	
}
