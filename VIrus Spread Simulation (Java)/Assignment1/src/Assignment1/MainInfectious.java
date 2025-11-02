package Assignment1;

import general.Replication;
import general.Simulation;
import general.automagic.AutoReplication;

public class MainInfectious 
{
	public static void main(String[] args) {
		double timeHorizon;
		int nReplication;
		long seed;

		// Baseline Scenario:
		InfectiousState state = new InfectiousState(timeHorizon, seed);
		Replication<InfectiousState> replication = new AutoReplication<InfectiousState>(state);
		Simulation<InfectiousState> simulation = new Simulation<>(replication);
		simulation.run(nReplications);
		// Print estimates prints the mean and standard error of all defined performance measures
		System.out.println("Baseline Performance:");
		simulation.printEstimates();

		System.out.println("----------------------------------------------");

		// Social Distancing Scenarios:
		for (int thresholdICU = 1; thresholdICU <= 5; thresholdICU = thresholdICU += 1) {
			state = new InfectiousState(timeHorizon, seed);
			replication = new AutoReplication<InfectiousState>(state);
			simulation = new Simulation<>(replication);
			simulation.run(nReplications);
			// Print estimates prints the mean and standard error of all defined performance measures
			System.out.println("Social Distancing:" + thresholdICU);
			simulation.printEstimates();

			System.out.println("----------------------------------------------");
		}

		// Vaccination Scenario:
		state = new InfectiousState(timeHorizon, seed);
		replication = new AutoReplication<InfectiousState>(state);
		simulation = new Simulation<>(replication);
		simulation.run(nReplications);
		// Print estimates prints the mean and standard error of all defined performance measures
		System.out.println("Vaccinating:");
		simulation.printEstimates();

		System.out.println("----------------------------------------------");

		// Social Distancing + Vaccination Scenarios:
		for (int thresholdICU = 1; thresholdICU <= 5; thresholdICU = thresholdICU += 1) {
			state = new InfectiousState(timeHorizon, seed);
			replication = new AutoReplication<InfectiousState>(state);
			simulation = new Simulation<>(replication);
			simulation.run(nReplications);
			// Print estimates prints the mean and standard error of all defined performance measures
			System.out.println("Social Distancing + Vaccinating:" + thresholdICU);
			simulation.printEstimates();
			
			System.out.println("----------------------------------------------");
		}
	}
}
