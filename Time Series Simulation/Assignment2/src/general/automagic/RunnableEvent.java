package general.automagic;

import general.Event;
import general.SystemState;

/**
 * Event that simply calls a {@link java.lang.Runnable} as action
 * to execute when process is called. This is mostly to create events based
 * on methods rather than having to implement your own Event
 * subclasses. 
 * 
 * @author Paul Bouman
 *
 * @param <S> the State used in the simulations
 */
public class RunnableEvent<S extends SystemState<S>> extends Event<S> {

	private Runnable action;
	
	public RunnableEvent(double time, Runnable action) {
		super(time);
		this.action = action;
	}

	@Override
	public void process(S state) {
		action.run();
	}

}
