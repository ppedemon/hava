package simple.arith.eval;

import simple.arith.exp.*;

/**
 * An expression visitor. Usefult for defining
 * generic iterators (very much like folds).
 *
 * @author: Pablo Pedemonte
 */
public interface ExpressionVisitor {
/**
 * Visit an addition expression.
 * 
 * @return an object resulting from this method's execution
 *
 * @param exp the expression to visit
 */
Object visit(Addition exp);
/**
 * Visit a constant expression.
 * 
 * @return an object resulting from this method's execution
 *
 * @param exp the expression to visit
 */
Object visit(Constant exp);
/**
 * Visit a division expression.
 * 
 * @return an object resulting from this method's execution
 *
 * @param exp the expression to visit
 */
Object visit(Division exp);
/**
 * Visit a negation expression.
 * 
 * @return an object resulting from this method's execution
 *
 * @param exp the expression to visit
 */
Object visit(Negation exp);
/**
 * Visit a product expression.
 * 
 * @return an object resulting from this method's execution
 *
 * @param exp the expression to visit
 */
Object visit(Product exp);
/**
 * Visit a substraction expression.
 * 
 * @return an object resulting from this method's execution
 *
 * @param exp the expression to visit
 */
Object visit(Substraction exp);
}
