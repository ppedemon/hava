package simple.arith.exp;

import simple.arith.eval.*;

/**
 * A division expression.
 *
 * @author: Pablo Pedemonte
 */
public final class Division extends BinaryExpression {
/**
 * Default constructor.
 */
protected Division() {
	super();
}
/**
 * Construct a new division expression.
 *
 * @param left  the left side subexpression
 * @param right the right side subexpression
 */
public Division(Expression left, Expression right) {
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
