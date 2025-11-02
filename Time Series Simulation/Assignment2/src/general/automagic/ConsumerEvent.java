package general.automagic;

import java.util.function.Consumer;

import general.Event;
import general.SystemState;

/**
 * Event that simply calls a {@link Consumer} with the time of the event
 * as it's process action. This is mostly to create events based
 * on methods rather than having to implement your own Event
 * subclasses. 
 * 
 * @author Paul Bouman
 *
 * @param <S> the State used in the simulations
 */
public class ConsumerEvent<S extends SystemState<S>> extends Event<S> {

	private Consumer<Double> consumer;
	
	public ConsumerEvent(double time, Consumer<Double> consumer) {
		super(time);
		this.consumer = consumer;
	}

	@Override
	public void process(S state) {
		consumer.accept(this.getTime());
	}

}
