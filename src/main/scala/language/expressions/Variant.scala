package language.expressions

import language.values.{Value, VariantValue}
import language.{Environment, Pattern, Store, SumTy, Type, UnitType, values}

case class VariantExpression(label: String, expr: Expression, ty: Type) extends Expression {
  override def typecheck(env: Environment): Type = {
    val optTy = ty.ensureIsType[SumTy].types.get(label)
    if (optTy.isEmpty) {
      throw new IllegalArgumentException(s"$label not a type in $ty")
    }
    val variantBodyType = expr.typecheck(env)
    if (variantBodyType != optTy.get) {
      throw new IllegalArgumentException(s"Expected variant body to have ${optTy.get}, found $variantBodyType")
    }
    ty
  }

  override def evaluate(store: Store): Value = values.VariantValue(label, expr.evaluate(store), ty)

  override protected def checkSub(other: VariantExpression.this.type): Boolean = {
    label.equals(other.label) && expr.sameShapeAs(other.expr) && ty.equals(other.ty)
  }

  override def substitute(variable: String, value: Value): Expression = VariantExpression(label, expr.substitute(variable, value), ty)
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

  override def checkSub(other: PatternMatch.this.type): Boolean = ???

  override def typecheck(env: Environment): Type = {
    val exprTy = expression.typecheck(env)
    var evaluatedType: Option[Type] = None
    for ((pattern, expr) <- cases) {
      val exprType = pattern.typeCheckBound(expr, env, exprTy)

      evaluatedType match {
        case None => evaluatedType = Some(exprType)
        case Some(value) => if (!value.equals(exprType)) {
          throw new IllegalArgumentException(s"All branches in must have the same type found [$exprType], expected [$value]")
        }
      }
    }
    evaluatedType match {
      case Some(ty) => ty
      case None => throw new IllegalArgumentException("Could not type check pattern match.")
    }
  }
}


/**
 * Gamma |- e : {l1:Tau1, l2: Tau2, ..., ln: Taun}
 * foreach i Gamma;x_i:Tau_i |- t_i : Tau
 * -----------------------------------------------
 *  Gamma |- match e with
 *      case l1(x1) => t1
 *      ...
 *      case ln(xn) => tn
 *  : Tau
 */
case class Match(expr: Expression, cases: Map[String, (String, Expression)]) extends Expression {
  override def evaluate(store: Store): Value = {
    val variant = expr.evaluate(store).ensureHasType[VariantValue]()
    val pair = cases(variant.label)
    pair._2.substitute(pair._1, variant.value).evaluate(store)
  }

  override def substitute(variable: String, value: Value): Expression = {
    Match(expr.substitute(variable, value),
      cases.map(kv => (kv._1, (kv._2._1, kv._2._2.substitute(variable, value)))))
  }

  override def typecheck(env: Environment): Type = {
    val variantTy = expr.typecheck(env).ensureIsType[SumTy]
    var evaluatedType: Option[Type] = Option.empty
    for ((label, ty) <- variantTy.types) {
      if (!cases.contains(label)) {
        throw new IllegalArgumentException(s"$label not contained in variant cases.")
      }
      val (binder, e) = cases(label)
      env.bind(binder, ty)
      val caseType = e.typecheck(env)
      env.unbind(binder)

      evaluatedType match {
        case Some(evTy) => assert(evTy == caseType)
        case None => evaluatedType = Some(caseType)
      }

    }
    evaluatedType.getOrElse(UnitType())
  }

  override protected def checkSub(other: Match.this.type): Boolean = ???
}


