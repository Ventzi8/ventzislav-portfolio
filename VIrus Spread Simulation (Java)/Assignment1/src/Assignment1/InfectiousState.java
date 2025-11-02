package Assignment1;

import general.SystemState;
import general.annotations.Initialize;

public class InfectiousState extends SystemState<InfectiousState>
{
	public InfectiousState(double timeHorizon, long seed) {
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
