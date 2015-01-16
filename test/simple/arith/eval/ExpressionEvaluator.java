package simple.arith.eval;

import simple.arith.exp.*;

/**
 * A simple expression evaluator.
 *
 * @author: Pablo Pedemonte
 */
public final class ExpressionEvaluator implements ExpressionVisitor {

	/** A dummy field for storing the evaluatiion result */
	private int res;
/**
 * Default constructor.
 */
public ExpressionEvaluator() {
	super();
}
/**
 * Evaluate an integer arithmetic expression.
 * 
 * @return the evaluation's result
 *
 * @param exp the expression to evaluate
 */
public int evaluate(Expression exp) {
	return ((IntWrapper)exp.accept(this)).getInt();
}
/**
 * Perform a simple test.
 * 
 * @param argv the argument vector. Unused.
 */
public static void main(String[] argv) {
	Addition a = 
		new Addition(
			new Substraction(
				new Division(
					new Constant(4),
					new Constant(2)
				),
				new Negation(
					new Constant(2)
				)
			),
			new Product(
				new Constant(3),
				new Constant(2)
			)
		);

	ExpressionEvaluator e = new ExpressionEvaluator();
	e.res = e.evaluate(a);
}
/**
 * Visit an addition expression.
 * 
 * @return an object resulting from this method's execution
 *
 * @param exp the expression to visit
 */
public Object visit(Addition exp) {
	IntWrapper l = (IntWrapper)exp.getLeft().accept(this);
	IntWrapper r = (IntWrapper)exp.getRight().accept(this);
	
	return new IntWrapper(l.getInt() + r.getInt());
}
/**
 * Visit a constant expression.
 * 
 * @return an object resulting from this method's execution
 *
 * @param exp the expression to visit
 */
public Object visit(Constant exp) {
	return new IntWrapper(exp.getValue());
}
/**
 * Visit a division expression.
 * 
 * @return an object resulting from this method's execution
 *
 * @param exp the expression to visit
 */
public Object visit(Division exp) {
	IntWrapper l = (IntWrapper)exp.getLeft().accept(this);
	IntWrapper r = (IntWrapper)exp.getRight().accept(this);
	
	return new IntWrapper(l.getInt() / r.getInt());
}
/**
 * Visit a negation expression.
 * 
 * @return an object resulting from this method's execution
 *
 * @param exp the expression to visit
 */
public Object visit(Negation exp) {
	IntWrapper s = (IntWrapper)exp.getSubExpression().accept(this);
	return new IntWrapper(-s.getInt());
}
/**
 * Visit a product expression.
 * 
 * @return an object resulting from this method's execution
 *
 * @param exp the expression to visit
 */
public Object visit(Product exp) {
	IntWrapper l = (IntWrapper)exp.getLeft().accept(this);
	IntWrapper r = (IntWrapper)exp.getRight().accept(this);
	
	return new IntWrapper(l.getInt() * r.getInt());
}
/**
 * Visit a substraction expression.
 * 
 * @return an object resulting from this method's execution
 *
 * @param exp the expression to visit
 */
public Object visit(Substraction exp) {
	IntWrapper l = (IntWrapper)exp.getLeft().accept(this);
	IntWrapper r = (IntWrapper)exp.getRight().accept(this);
	
	return new IntWrapper(l.getInt() - r.getInt());
}
}
