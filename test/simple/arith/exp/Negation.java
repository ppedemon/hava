package simple.arith.exp;

import simple.arith.eval.*;

/**
 * A negation subexpression
 *
 * @author: Pablo Pedemonte
 */
public class Negation extends UnaryExpression {
/**
 * Negation constructor comment.
 */
protected Negation() {
	super();
}
/**
 * Create a new negation expression with the 
 * given subexpression
 * 
 * @param subExpression the subexpression for this
 * expression
 */
public Negation(Expression subExpression) {
	super(subExpression);
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
