package simple.arith.exp;

/**
 * A unary integer arithmetic expression.
 *
 * @author: Pablo Pedemonte
 */
public abstract class UnaryExpression extends Expression {

	/* the subexpression associated to this class */
	private Expression subExpression;
/**
 * UnaryExpression constructor comment.
 */
protected UnaryExpression() {
	super();
}
/**
 * Create a new unary expression with the 
 * given subexpression
 * 
 * @param subExpression the subexpression for this
 * expression
 */
public UnaryExpression(Expression subExpression) {
	super();
	this.setSubExpression(subExpression);
}
/**
 * Return this this expression's subexpression.
 * 
 * @return this expression's subexpression
 */
public Expression getSubExpression() {
	return subExpression;
}
/**
 * Set this expression's subexpression
 * 
 * @param newSubExpression the new subexpression to set
 */
public void setSubExpression(Expression newSubExpression) {
	subExpression = newSubExpression;
}
}
