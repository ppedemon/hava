package simple.arith.exp;

import simple.arith.eval.*;

/**
 * An addition expression.
 *
 * @author: Pablo Pedemonte
 */
public final class Addition extends BinaryExpression {
/**
 * Default constructor.
 */
protected Addition() {
	super();
}
/**
 * Construct a new addition expression.
 *
 * @param left  the left side subexpression
 * @param right the right side subexpression
 */
public Addition(Expression left, Expression right) {
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
