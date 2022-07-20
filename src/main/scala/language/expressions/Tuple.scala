package language.expressions

import language.values.{TupleValue, Value}
import language.{Environment, ProductTy, Store, Type}

case class TupleExpression(exprs: List[Expression]) extends Expression  {

  override def typecheck(env: Environment): Type = ProductTy(exprs.map(_.typecheck(env)))

  override def evaluate(store: Store): Value = TupleValue(exprs.map(_.evaluate(store)))

  override def substitute(variable: String, value: Value): Expression = TupleExpression(exprs.map(_.substitute(variable, value)))

  override def typeSubs(typeVar: String, ty: Type): Expression = TupleExpression(exprs.map(_.typeSubs(typeVar, ty)))

  override def sameShapeAs(other: Expression): Boolean = {
    other match {
      case TupleExpression(otherExprs) => checkExpressions(otherExprs)
      case TupleValue(otherExprs) => checkExpressions(otherExprs)
      case _ => false
    }
  }

  override protected def checkSub(other: TupleExpression.this.type): Boolean = {
    checkExpressions(other.exprs)
  }

  private def checkExpressions(otherExprs: List[Expression]): Boolean = {
    exprs.length == otherExprs.length && exprs.indices.forall(index => exprs(index).sameShapeAs(otherExprs(index)))
  }
  override def toString: String = exprs.map(_.toString).mkString("[",", ","]")
}


/**
 * A tuple projection.
 *
 * Gamma |- e : Tau1 * Tau 2 * ... * Tau n      1 <= i <= n
 * ----------------------------------------------------------
 *                 Gamma |- e.i : Tau i
 *
 */
case class Projection(e: Expression, index: Int) extends Expression {
  override def evaluate(store: Store): Value = {
    val tuple = e.evaluate(store).ensureHasType[TupleValue]()
    tuple.values(index)
  }

  override def substitute(variable: String, value: Value): Expression = Projection(e.substitute(variable, value), index)

  override def typeSubs(typeVar: String, ty: Type): Expression = Projection(e.typeSubs(typeVar, ty), index)
  override def typecheck(env: Environment): Type = {
    e.typecheck(env) match {
      case ProductTy(types) => types(index)
      case _ => throw new IllegalArgumentException("Expected product type in projection.")
    }
  }

  override protected def checkSub(other: Projection.this.type): Boolean = {
    e.sameShapeAs(other.e) && index == other.index
  }

  override def toString: String = e.toString + "." + index.toString
}

