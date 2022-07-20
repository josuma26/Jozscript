package language.expressions

import language._
import language.values.Value

/**
 * Expressions in the language.
 * x | \x:T, e | e e | e +-*% e | e &&|| e
 *
 */
trait Expression {

  def evaluate(store: Store): Value

  def substitute(variable: String, value: Value): Expression

  def typecheck(env: Environment): Type

  def sameShapeAs(other: Expression): Boolean = {
    if (this.getClass.equals(other.getClass)) {
      checkSub(other.asInstanceOf[this.type])
    } else {
      false
    }
  }

  def typeSubs(typeVar: String, ty: Type): Expression

  protected def checkSub(other: this.type): Boolean



  def ensureHasType[T <: Expression](): T = {
    this match {
      case e: T => e
      case _ => throw new IllegalStateException(s"Expected $this to be of a different tyoe.")
    }
  }

}

case class Sequence(s1: Expression, s2: Expression) extends Expression {
  override def evaluate(store: Store): Value = {
    s1.evaluate(store); s2.evaluate(store)
  }

  override def substitute(variable: String, value: Value): Expression = {
    Sequence(s1.substitute(variable, value), s2.substitute(variable, value))
  }

  override def typecheck(env: Environment): Type = {
    s1.typecheck(env)
    s2.typecheck(env)
  }

  override def typeSubs(typeVar: String, ty: Type): Expression = {
    Sequence(s1.typeSubs(typeVar, ty), s2.typeSubs(typeVar, ty))
  }

  override def toString: String = s1.toString + ";\n" + s2.toString

  override def checkSub(other: Sequence.this.type): Boolean = {
    this.s1.sameShapeAs(other.s1) && this.s2.sameShapeAs(other.s2)
  }
}