package general;

/**
 * Abstract class representing a performance measure. After running {@link Simulation}, the user has access to the 
 * {@link #getMean()} and {@link #getStandardError()} methods.
 * 
 * @author Nemanja Milovanovic
 *
 */

public abstract class PerformanceMeasure<S extends SystemState<S>> {

	private final String name;
	private Double mean;
	private Double std;
	
	public PerformanceMeasure(String name) {
		this.name = name;
	}
	
	/**
	 * Returns the performance measure for iteration {@code i}, given the statistical {@code Counter}s present in the model. 
	 * 
	 * @param state	The state for which we computer the performance measure
	 * @return the measured value of this performance measure based on the state
	 */
	public abstract double compute(S state);
	
	/**
	 * @return	Returns the estimate of the performance measure
	 */
	public double getMean() {
		if (mean == null) {
			throw new IllegalAccessError("The simulation has not been run yet");
		}
		return mean;
	}
	
	/**
	 * @return	Returns the standard error of the performance measure
	 */
	public double getStandardError() {
		if (std == null) {
			throw new IllegalAccessError("The simulation has not been run yet");
		}
		return std;
	}
	
	/**
	 * Sets the performance measure estimate
	 * 
	 * @param mean	Estimate
	 */
	public void setMean(double mean) {
		this.mean = mean;
	}
	
	/**
	 * Sets the performance measure standard error
	 * 
	 * @param std	Standard error
	 */
	public void setStandardError(double std) {
		this.std = std;
	}
	
	/**
	 * @return Returns the name of the performance measure
	 */
	public String getName() {
		return name;
	}
	
	@Override
	public String toString() {
		return name + ": " + mean + " (" + std + ")";
	}
}
