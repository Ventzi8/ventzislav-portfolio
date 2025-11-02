package Assignment1;

import general.SystemState;
import general.annotations.Initialize;

public class FerryState extends SystemState<FerryState>
{
	public FerryState(double timeHorizon, long seed, double time) {
		super(timeHorizon, seed);
		
		reset();
	}
	
	@Initialize
	public void initReplication() {
		
	}
	
	@Override
	public void reset() {
		
	}
}
