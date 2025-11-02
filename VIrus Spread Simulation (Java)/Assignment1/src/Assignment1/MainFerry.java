package Assignment1;

import general.Replication;
import general.Simulation;
import general.automagic.AutoReplication;

public class MainFerry 
{
	public static void main(String[] args) {
		double timeHorizon;
		int nReplications;
		long seed;
		
		for (double time = 5; time < 20; time = time + 2.5) {
			FerryState state = new FerryState(timeHorizon, seed, time);
			Replication<FerryState> replication = new AutoReplication<FerryState>(state);

			Simulation<FerryState> simulation = new Simulation<>(replication);
			simulation.run(nReplications);
			// Print estimates prints the mean and standard error of all defined performance measures
			System.out.println("Time: " + time);
			simulation.printEstimates();
		}
	}
}
