package language.expressions

import language.values.Value
import language.{Environment, Store, Type}

/**
 * A variable.
 *
 * Gamma(x)= T
 * -----------
 * Gamma |- x : Tau
 *
 */
case class Var(variable: String) extends Expression {
  override def evaluate(store: Store): Value = store.lookup(variable)

  override def typecheck(env: Environment): Type = env.get(variable)

  override def substitute(variable: String, value: Value): Expression = {
    if (this.variable.equals(variable)) value else this
  }

  override def sameShapeAs(other: Expression): Boolean = {
    true
  }

  override protected def checkSub(other: Var.this.type): Boolean = true

  override def toString: String = variable
}
