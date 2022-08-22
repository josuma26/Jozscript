package language.expressions

import hoarelogic.logic.{And, Proposition}
import language.values.Value
import language._

case class VariantExpression(label: String, expr: Expression, ty: Type) extends Expression {
  override def typecheck(env: Environment): Type = {
    val optTy = ty.getIfAlias(env).ensureIsType[SumTy](env).types.get(label)
    if (optTy.isEmpty) {
      throw new IllegalArgumentException(s"$label not a type in $ty")
    }
    val variantBodyType = expr.typecheck(env)
    if (!variantBodyType.eq(optTy.get, env)) {
      throw new IllegalArgumentException(s"Expected variant body to have ${optTy.get}, found $variantBodyType")
    }
    ty
  }

  override def evaluate(store: Store): Value = values.VariantValue(label, expr.evaluate(store), ty)

  override protected def checkSub(other: VariantExpression.this.type): Boolean = {
    label.equals(other.label) && expr.sameShapeAs(other.expr) && ty.equals(other.ty)
  }

  override def substitute(variable: String, value: Value): Expression =
    VariantExpression(label, expr.substitute(variable, value), ty)

  override def replace(variable: String, value: Expression): Expression =
    VariantExpression(label, expr.replace(variable, value), ty)

  override def typeSubs(typeVar: String, ty: Type): Expression =
    VariantExpression(label, expr.typeSubs(typeVar, ty), this.ty)

  override def printCoq(): String = label + "(" + expr.printCoq() + ")"
}

/**
 * General pattern matching that can be done on any expression.
 *
 * match e with {
 *  | [pattern] => e1
 *  | [pattern] => e2
 *  ...
 *  }
 *
 */
case class PatternMatch(expression: Expression, cases: List[(Pattern, Expression)]) extends Expression {
  override def evaluate(store: Store): Value = {
    val value = expression.evaluate(store)
    cases.foreach({
      case (pattern, expression) =>
        if (pattern.matches(value, store)) {
          return pattern.substituteInto(expression, value).evaluate(store)
        }
    })
    throw new IllegalArgumentException(s"No pattern matched $expression")
  }

  override def substitute(variable: String, value: Value): Expression = {
    PatternMatch(
      expression.substitute(variable, value),
      cases.map({
        case (patt, expr) => (patt, expr.substitute(variable, value))
      })
    )
  }

  override def replace(variable: String, value: Expression): Expression = {
    PatternMatch(
      expression.replace(variable, value),
      cases.map({
        case (patt, expr) => (patt, expr.replace(variable, value))
      })
    )
  }

  override def typeSubs(typeVar: String, ty: Type): Expression = {
    PatternMatch(
      expression.typeSubs(typeVar, ty),
      cases.map({
        case (patt, expr) => (patt, expr.typeSubs(typeVar, ty))
      })
    )
  }

  override def checkSub(other: PatternMatch.this.type): Boolean = ???

  override def typecheck(env: Environment): Type = {
    val exprTy = expression.typecheck(env)
    var evaluatedType: Option[Type] = None
    for ((pattern, expr) <- cases) {
      val exprType = pattern.typeCheckBound(expr, env, exprTy)

      evaluatedType match {
        case None => evaluatedType = Some(exprType)
        case Some(value) => if (!value.eq(exprType, env)) {
          throw new IllegalArgumentException(s"All branches in must have the same type found [$exprType], expected [$value]")
        }
      }
    }
    evaluatedType match {
      case Some(ty) => ty
      case None => throw new IllegalArgumentException("Could not type check pattern match.")
    }
  }

  override def proofObligation(pre: Proposition, post: Proposition): Proposition = {
    cases.map({
      case (patt, e) => e.proofObligation(And(pre, patt.matchProp(expression)), post)
    }).reduce(And)
  }

  override def pushThrough(pre: Proposition): Proposition = {
    cases.map({
      case (patt, e) => e.pushThrough(And(pre, patt.matchProp(expression)))
    }).reduce(And)
  }

  override def printCoq(): String = {
    "match " + expression.printCoq() + " with \n" + cases.map({
      case (pattern, expr) => "\t| " + pattern.printCoq() + " => " + expr.printCoq()
    }).mkString("\n") + "\n\tend"
  }
}



