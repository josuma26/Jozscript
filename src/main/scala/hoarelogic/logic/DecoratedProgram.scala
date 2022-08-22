package hoarelogic.logic

import language.expressions.Expression
import language.values.Value
import language.{Environment, Store, Type}

case class DecoratedProgram(pre: Proposition, prog: Expression, post: Proposition) extends Expression {
  override def evaluate(store: Store): Value = prog.evaluate(store)

  override def substitute(variable: String, value: Value): Expression = prog.substitute(variable, value)

  override def replace(variable: String, expr: Expression): Expression = prog.replace(variable, expr)

  override def typecheck(env: Environment): Type = { println(prog.proofObligation(pre, post)) ; prog.typecheck(env) }

  override def typeSubs(typeVar: String, ty: Type): Expression = prog.typeSubs(typeVar, ty)

  override protected def checkSub(other: DecoratedProgram.this.type): Boolean = prog.sameShapeAs(other.prog)

  override def printCoq(): String = {
    "(*\n" + "{{" + pre.toString + "}}\n\n" + prog.toString() + "\n\n{{" + post.toString + "}}\n*)\n\n" +
    "Theorem correct: " + prog.proofObligation(pre, post) + ".\n\tProof.\nAdmitted."
  }
}
