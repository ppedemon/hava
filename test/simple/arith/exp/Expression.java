package simple.arith.exp;

import simple.arith.eval.*;

/**
 * An abstract integer arithmetic expression.
 *
 * @author: Pablo Pedemonte
 */
public abstract class Expression {
/**
 * Default constructor.
 */
protected Expression() {
	super();
}
/**
 * Accept the given visitor.
 * 
 * @return the object resulting from accepting the visitor
 *
 * @param visitor the visitor to be accepted
 */
public abstract Object accept(ExpressionVisitor visitor);
}
