package simple.arith.exp;

import simple.arith.eval.*;

/**
 * A constant integer expression.
 *
 * @author: Pablo Pedemonte
 */
public final class Constant extends Expression {
	private int value;
/**
 * Default constructor.
 */
protected Constant() {
	super();
}
/**
 * Creaetea new constant expression with a given value.
 * 
 * @param newValue the initial value for this constant
 */
public Constant(int newValue) {
	super();
	this.setValue(newValue);
}
/**
 * Accept the given visitor.
 * 
 * @return the object resulting from accepting the visitor
 *
 * @param visitor the visitor to be accepted
 */
public Object accept(ExpressionVisitor visitor) {
	return visitor.visit(this);
}
/**
 * Get this constant's value.
 * 
 * @return this constant's value
 */
public int getValue() {
	return value;
}
/**
 * Set this constant's value.
 * 
 * @param newValue the new value for this constant
 */
public void setValue(int newValue) {
	value = newValue;
}
}
