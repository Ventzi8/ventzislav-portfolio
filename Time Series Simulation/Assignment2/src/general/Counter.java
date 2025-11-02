package general;

/**
 * This class is used to model statistical counters. 
 * 
 * As it is the most prevalent, this class models an accumulating statistical counter. 
 * 
 * @author Nemanja Milovanovic
 *
 */
public class Counter {

	private double initialValue;
	private double accumSum;
	private String name;
	
	public Counter(
			double initialValue, 
			String name) {
		this.initialValue = initialValue;
		accumSum = initialValue;
		this.name = name;
	}
	
	/**
	 * Increment the accumulated sum by <code>x</code>
	 * 
	 * @param x Value to add
	 */
	public void incrementBy(double x) {
		accumSum += x;
	}
	
	/**
	 * Increment the accumulated sum by 1.
	 */
	public void increment() {
		incrementBy(1);
	}
	
	/**
	 * 
	 * @return Returns the current value of the accumulated sum
	 */
	public double getValue() {
		return accumSum;
	}
	
	/**
	 * Resets the counter to its initial value.
	 */
	public void reset() {
		accumSum = initialValue;
	}
	
	@Override
	public String toString() {
		return "[" + name + ": " + accumSum + "]";
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		long temp;
		temp = Double.doubleToLongBits(accumSum);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof Counter))
			return false;
		Counter other = (Counter) obj;
		if (Double.doubleToLongBits(accumSum) != Double.doubleToLongBits(other.accumSum))
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}
}
