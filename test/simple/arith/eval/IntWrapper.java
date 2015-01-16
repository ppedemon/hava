package simple.arith.eval;

/**
 * A dummy wrapper for primitive ints.
 *
 * @author: Pablo Pedemonte
 */
public final class IntWrapper {
	private int i;
/**
 * Make this private, so we can't create
 * an empty wrapper.
 */
private IntWrapper() {
	super();
}
/**
 * Construct a wrapper for the given integer. 
 */
public IntWrapper(int i) {
	super();
	this.setInt(i);
}
/**
 * Get this wrapper's int value
 * 
 * @return this wrapper's int value
 */
public int getInt() {
	return i;
}
/**
 * Set this wrapper's int value.
 * 
 * @param newValue the new value fore this wrapper
 */
public void setInt(int newValue) {
	i = newValue;
}
}
