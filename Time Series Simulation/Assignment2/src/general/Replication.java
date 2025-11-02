package general;

import java.util.PriorityQueue;

/**
 * This class represents one replication of a Discrete-Event Simulation (DES).
 * 
 * Note that each time we do a replication, we call first {@link #reset()}, and then {@link #initialize()}. 
 * 
 * @author Nemanja Milovanovic
 *
 */
public abstract class Replication<S extends SystemState<S>> {
	
	private final S state;
	private Status status;
	
	public Replication(S state) {
		this.state = state;
	}
	
	/**
	 * This method should initialize the DES, such that it is ready to <code>run</code> afterwards. 
	 * In most basic implementations, this means inserting the first arrival into the queue.
	 */
	public abstract void initialize();
	
	/**
	 * This method starts the simulation, assuming it has been initialized. 
	 * 
	 * First we extract the next <code>Event</code> form the queue, we process it, then extract the next one, and so on. 
	 * The simulation is terminated when <code>shouldTerminate</code> evaluates to true. Note that <code>shouldTerminate</code> 
	 * is run after the event is retrieved, but before it is processed. After we process an event, we set the current time to the time of the event. 
	 */
	public void run() {
		PriorityQueue<Event<S>> queue = state.getQueue();
		while (true) {
			Event<S> e = queue.poll();
			if (e == null) {
				status = Status.FAILED;
				throw new IllegalStateException("The event queue is empty before the official termination criterion has been satisfied.");
			}
			
			// check if we should terminate due to time
			if (state.passedTimeHorizon(e)) {
				status = Status.TIME_TERMINATED;
				break;
			}
			
			// check if we should terminate by user
			if (shouldTerminate(state)) {
				status = Status.USER_TERMINATED;
				break;
			}
			
			e.process(state);
			double newTime = e.getTime();
			state.updateCurrentTime(newTime);
		}
	}
	
	/**
	 * This method allows the user to specify some own termination criterion, in addition to surpassing the simulation time horizon.
	 * 
	 * @param state System state
	 * @return	True if the simulation should terminate according to some user-specified criterion, false otherwise
	 */
	public abstract boolean shouldTerminate(S state);
	
	/**
	 * 
	 * @return	Returns the simulation status. See {@link general.Status}
	 */
	public Status getStatus() {
		return status;
	}
	
	/**
	 * 
	 * @return Returns the current system state
	 */
	public S getSystemState() {
		return state;
	}
	
	/**
	 * Resets the event queue, simulation clock, counters, and system state
	 */
	public void reset() {
		// This step slow down the replication, but is more safe
		state.injectCounters();
		state.updateCurrentTime(0);
		for (Counter c : state.getCounters()) {
			c.reset();
		}
		state.reset();
	}
}
