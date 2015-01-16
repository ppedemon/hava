package simple.arith.exp;

import simple.arith.eval.*;

/**
 * A subtraction expression.
 *
 * @author: Pablo Pedemonte
 */
public final class Substraction extends BinaryExpression {
/**
 * Default constructor.
 */
protected Substraction() {
	super();
}
/**
 * Construct a new substraction expression.
 *
 * @param left  the left side subexpression
 * @param right the right side subexpression
 */
public Substraction(Expression left, Expression right) {
	super(left, right);
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
}
