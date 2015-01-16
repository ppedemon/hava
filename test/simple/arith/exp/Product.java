package simple.arith.exp;

import simple.arith.eval.*;

/**
 * A product expression.
 *
 * @author: Pablo Pedemonte
 */
public final class Product extends BinaryExpression {
/**
 * Default constructor.
 */
protected Product() {
	super();
}
/**
 * Construct a new product expression.
 *
 * @param left  the left side subexpression
 * @param right the right side subexpression
 */
public Product(Expression left, Expression right) {
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
