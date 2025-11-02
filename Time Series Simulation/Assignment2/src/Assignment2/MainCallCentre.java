package Assignment2;

import general.Replication;
import general.Simulation;
import general.automagic.AutoReplication;

public class MainCallCentre 
{
	public static void main(String[] args) {
		double timeHorizon;
		int nReplications;
		long seed;
		
		CallCentreState state = new CallCentreState(timeHorizon, seed);
		Replication<CallCentreState> replication = new AutoReplication<CallCentreState>(state);

		Simulation<CallCentreState> simulation = new Simulation<>(replication);
		simulation.run(nReplications);
		// Print estimates prints the mean and standard error of all defined performance measures
		System.out.println("Hire additional general employees: ");
		simulation.printEstimates();
		
		
		state = new CallCentreState(timeHorizon, seed);
		replication = new AutoReplication<CallCentreState>(state);

		simulation = new Simulation<>(replication);
		simulation.run(nReplications);
		// Print estimates prints the mean and standard error of all defined performance measures
		System.out.println("Hire specific employees: ");
		simulation.printEstimates();
	}
}