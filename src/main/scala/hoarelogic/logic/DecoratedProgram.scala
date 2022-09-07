package hoarelogic.logic

import language.expressions.Expression
import language.values.Value
import language.{Environment, Store, Type}

case class DecoratedProgram(pre: Proposition, prog: Expression, post: Proposition) extends Expression {
  override def evaluate(store: Store): Value = prog.evaluate(store)

  override def substitute(variable: String, value: Value): Expression = prog.substitute(variable, value)

  override def replace(variable: String, expr: Expression): Expression = prog.replace(variable, expr)

  override def typecheck(env: Environment): Type = prog.typecheck(env)

  override def typeSubs(typeVar: String, ty: Type): Expression = prog.typeSubs(typeVar, ty)

  override protected def checkSub(other: DecoratedProgram.this.type): Boolean = prog.sameShapeAs(other.prog)

  override def printCoq(env: Environment): String = {
    val po = prog.proofObligation(pre, post, env)
    "(*\n" + "{{" + pre.printCoq(env) + "}}\n\n" + prog.toString + "\n\n{{" + post.printCoq(env) + "}}\n*)\n\n" +
    "Theorem correct: " + po.quantify().printCoq(env) + ".\n\tProof.\nAdmitted."
  }
}
