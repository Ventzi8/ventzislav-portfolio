package Assignment2;


import general.SystemState;
import general.annotations.Initialize;

public class CallCentreState extends SystemState<CallCentreState>
{
	public CallCentreState(double timeHorizon, long seed) {
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