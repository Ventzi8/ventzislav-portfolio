package general;

/**
 * This is an abstract class for events in a Discrete-Event Simulation (DES).
 * 
 * @author Nemanja Milovanovic
 *
 * @param <S> A user-defined system state class.
 */

public abstract class Event<S extends SystemState<S>> implements Comparable<Event<S>> {

	private double time;
	
	public Event(double time) {
		this.time = time;
	}
	
	/**
	 * 
	 * @return	Returns the time this event is associated to
	 */
	public double getTime() {
		return time;
	}
	
	@Override
	public int compareTo(Event<S> other) {
		int res = Double.compare(time, other.time);
		return res;
	}
	
	/**
	 * Processes the event by changing the system state and updating counters
	 * 
	 * @param state	System state
	 */
	public abstract void process(S state);

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		long temp;
		temp = Double.doubleToLongBits(time);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Event))
			return false;
		Event<?> other = (Event<?>) obj;
		if (Double.doubleToLongBits(time) != Double.doubleToLongBits(other.time))
			return false;
		return true;
	}
	
	
}
