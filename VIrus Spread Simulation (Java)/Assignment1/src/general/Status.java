package general;

/**
 * {@link Enum} type which is used to display the current {@link Replication} or {@link Simulation} status.
 * 
 * @author Nemanja Milovanovic
 *
 */
public enum Status {

	/**
	 * The {@link Replication} or {@link Simulation} is still running.
	 */
	ACTIVE, 
	
	/**
	 * The {@link Replication} or {@link Simulation} failed somehow.
	 */
	FAILED, 
	
	/**
	 * The {@link Simulation} terminated successfully.
	 */
	SUCCESS, 
	
	/**
	 * The {@link Replication} is terminated due to an {@link Event} surpassing the simulation time horizon.
	 */
	TIME_TERMINATED, 
	
	/**
	 * The {@link Replication} is terminated due to fulfilling {@link Replication#shouldTerminate(SystemState)}.
	 */
	USER_TERMINATED
}
