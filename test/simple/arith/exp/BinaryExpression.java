package simple.arith.exp;

/**
 * A binary integer arithmetic expression.
 *
 * @author: Pablo Pedemonte
 */
public abstract class BinaryExpression extends Expression {

	/* left and right side expressions */
	private Expression left;
	private Expression right;
/**
 * Default constructor.
 */
protected BinaryExpression() {
	super();
}
/**
 * Construct a new binary expression with
 * the given subexpressions.
 * 
 * @param left  the left side subexpression
 * @param right the right side subexpression
 */
public BinaryExpression(Expression left, Expression right) {
	super();
	this.setLeft(left);
	this.setRight(right);
}
/**
 * Get the left side expression.
 * 
 * @return the left side expression
 */
public Expression getLeft() {
	return left;
}
/**
 * Get the right side expression.
 * 
 * @return the right side expression
 */
public Expression getRight() {
	return right;
}
/**
 * Set the left side expression.
 * 
 * @param newLeft the new left side expression
 */
public void setLeft(Expression newLeft) {
	left = newLeft;
}
/**
 * Set the right side expression.
 * 
 * @param newRight the new right side expression
 */
public void setRight(Expression newRight) {
	right = newRight;
}
}
