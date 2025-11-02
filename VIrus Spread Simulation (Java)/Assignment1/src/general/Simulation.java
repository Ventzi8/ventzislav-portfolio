package general;

import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

/**
 * The main class of the template. <code>Simulation</code> allows the user to run multiple {@link Replication} instances and compute the simulation 
 * estimates of the performance measures. Furthermore it is possible to print the results to the {@link OutputStream} of choice, be it the standard 
 * {@link System#out} or one that writes to a file. 
 * 
 * @author Nemanja Milovanovic
 *
 */
public class Simulation<S extends SystemState<S>> {

	private List<PerformanceMeasure<S>> measures;
	private Replication<S> replication;
	private Status status;
	
	public Simulation(Replication<S> replication) {
		this(replication, replication.getSystemState().getAutomaticMeasures());
	}
	
	public Simulation(
			Replication<S> replication, 
			List<PerformanceMeasure<S>> measures) {
		this.replication = replication;
		// Copy and sort the performance measures to make output always consistent
		this.measures = new ArrayList<>(measures);
		this.measures.sort((pm1, pm2) -> pm1.getName().compareTo(pm2.getName()));
	}
	
	/**
	 * Runs the simulation that the user specified in {@link Replication} <code>n</code> times.
	 * 
	 * @param n Number of simulation iterations
	 */
	public void run(long n) {
		if (n < 1) {
			status = Status.FAILED;
			throw new IllegalArgumentException("Number of iterations must be >= 1");
		}
		
		// initialize matrix of performance measures
		List<List<Double>> measures = new ArrayList<>();
		
		long currentIteration = 0;
		while (currentIteration < n) {
			replication.reset();
			replication.initialize();
			replication.run();
			
			if (replication.getStatus() == Status.FAILED) {
				throw new IllegalStateException("Replication #" + (currentIteration + 1) + " failed.");
			}
			
			// compute the performance measures
			List<Double> rowMeasures = computePerformanceMeasures();
			measures.add(rowMeasures);
			currentIteration++;
		}
		
		// compute estimations of performance measures
		computePerformanceMeasureEstimates(measures);
		
		status = Status.SUCCESS;
	}
	
	/**
	 * 
	 * @return Returns the status of the DES.
	 */
	public Status getStatus() {
		return status;
	}
	
	private List<Double> computePerformanceMeasures() {
		List<Double> estimates = new ArrayList<>();
		for (PerformanceMeasure<S> measure : measures) {
			S state = replication.getSystemState();
			double estimate = measure.compute(state);
			estimates.add(estimate);
		}
		return estimates;
	}
	
	private void computePerformanceMeasureEstimates(List<List<Double>> estimates) {
		List<Double> first = estimates.get(0);
		int nrMeasures = first.size();
		for (int j = 0; j < nrMeasures; j++) {
			double sum = 0;
			for (int i = 0; i < estimates.size(); i++) {
				double d = estimates.get(i).get(j);
				sum += d;
			}
			double mean = sum/estimates.size();
			
			sum = 0;
			for (int i = 0; i < estimates.size(); i++) {
				double d = estimates.get(i).get(j);
				sum += Math.pow(d - mean, 2);
			}
			double std = Math.sqrt(sum/(estimates.size()-1));
			std = std/Math.sqrt(estimates.size());
			
			PerformanceMeasure<S> pm = measures.get(j);
			pm.setMean(mean);
			pm.setStandardError(std);
		}
	}
	
	public void printEstimates() {
		PrintWriter pw = new PrintWriter(System.out);
		printEstimates(pw);
		pw.flush();
	}
	
	public void printEstimates(PrintStream out) {
		for (int i = 0; i < measures.size(); i++) {
			out.println(measures.get(i));
		}
	}
	
	public void printEstimates(PrintWriter out) {
		for (int i = 0; i < measures.size(); i++) {
			out.println(measures.get(i));
		}
	}
}
