package language

import hoarelogic.logic._
import language.expressions._
import language.values._

/**
 * For pattern matching on values
 *
 * Patterns will have an associated type, which will be used to bind the pattern variables in the environment to
 * type-check the expression.
 *
 * For example:
 *
 * let x := two:[1,2] as {two:[Nat*Nat], num:Nat}
 * match x with {
 *  | two:[a,b] => a + b
 *  | num: n => n
 * }
 *
 * The pattern [a,b] has type [Nat*Nat] and the pattern n has type Nat, types come by looking up the label in
 * the variant type
 *
 * LabelBinderPattern: | label:Pattern => e
 *  - value matches if value is a Variant (l:e as {l:Tau....}), label == l, and Pattern.matches(value)
 *  - e is type checked by recursively calling Pattern.typecheckBound(value) (lable is ignored)
 *
 * ValuePattern: Value: | Value => e
 *  - value matches if value == Value
 *  - e is type checked by typechecking e in the env
 *
 *
 */
trait Pattern {
  def matches(value: Value, store: Store): Boolean

  def substituteInto(expr: Expression, value: Value): Expression

  /**
   * Typecheck the expression by substituing all variables in the pattern with the right type
   */
  def typeCheckBound(expr: Expression, env: Environment, valueType: Type): Type


  def matchProp(value: Expression, env: Environment): Proposition

  def printCoq(env: Environment): String

  def bindFreeVariables(patternValue: Expression, env: Environment, valueType: Type): Unit = {
    patternValue match {
      case Num(_) | Bool(_) | UnitVal() =>
      case Func(arg, ty, body) => ???
      case TupleValue(values) => bindTuple(values, env, valueType)
      case TupleExpression(exprs) => bindTuple(exprs, env, valueType)
      case Var(varName) => env.bindIfFree(varName, valueType)
      case VariantValue(label, value, _) => ???
      case TypeApp(e, ty) => bindFreeVariables(e, env, valueType)
      case App(e1, e2) => bindFreeVariables(e1, env, valueType); bindFreeVariables(e2, env, valueType)
      case pa => throw new IllegalArgumentException(s"Invalid pattern $pa")
    }
  }

  private def bindTuple(values: List[Expression], env: Environment, valueType: Type): Unit = {
    val tupleTypes = valueType.ensureIsType[ProductTy](env).types
    if (values.length != tupleTypes.length) {
      throw new IllegalArgumentException("Value cardinality mismath. This branch should not have matched.")
    }
    values.indices.foreach(index => bindFreeVariables(values(index), env, tupleTypes(index)))
  }

  def unbindFreeVariables(value: Expression, env: Environment): Unit = {
    value match {
      case Num(_) | Bool(_) | UnitVal() =>
      case Func(arg, ty, body) => ???
      case TupleValue(values) =>
        values.indices.foreach(index => unbindFreeVariables(values(index), env))
      case TupleExpression(values) => values.indices.foreach(index => unbindFreeVariables(values(index), env))
      case Var(varName) => env.unbind(varName)
      case VariantValue(label, value, _) => ???
      case TypeApp(e, ty) => unbindFreeVariables(e, env)
      case App(e1, e2) => unbindFreeVariables(e1, env); unbindFreeVariables(e2, env)

    }
  }

}

/**
 * l:[Pattern] => e
 */
case class LabelBinderPattern(label: String, binder: Pattern) extends Pattern {
  override def matches(value: Value, store: Store): Boolean = {
    val asVariant = value.ensureHasType[VariantValue]()
    asVariant.label == label && binder.matches(asVariant.value, store)
  }

  override def substituteInto(expr: Expression, value:   Value): Expression = {
    val asVariant = value.ensureHasType[VariantValue]()
    if (asVariant.label != label) {
      throw new IllegalArgumentException(s"$label is not equal to ${asVariant.label}, should not have matched.")
    }
    binder.substituteInto(expr, value.ensureHasType[VariantValue]().value)
  }

  override def typeCheckBound(expr: Expression, env: Environment, valueType: Type): Type = {
    val optTy = valueType.getIfAlias(env).ensureIsType[SumTy](env).types.get(label)
    if (optTy.isEmpty) {
      throw new IllegalArgumentException(s"$label not a constructor.")
    }
    binder.typeCheckBound(expr, env, optTy.get)
  }

  override def matchProp(value: Expression, env: Environment): Proposition = {
    value.typecheck(env).getIfAlias(env) match {
      case SumTy(types) => binder match {
        case ExpressionPattern(patternValue) => bindFreeVariables(patternValue, env, types(label))
      }
    }
    binder.matchProp(value, env) match {
      case ExprEq(e1, e2) => e2.typecheck(env).getIfAlias(env) match {
        case SumTy(types) => And(getAllExprHasTypes(types(label), e1),
        ExprEq(e2, VariantExpression(label, e1, types(label))))
      }
    }
  }

  private def getAllExprHasTypes(ty: Type, expr: Expression): Proposition = {
    (ty, expr) match {
      case (ProductTy(tys), TupleExpression(exprs)) =>
        if (tys.length != exprs.length) {
          throw new IllegalArgumentException("Need an equal amount of types and expression in tuple pattern.")
        }
        tys.indices.foldRight[Proposition](True())((i, p) => And(getAllExprHasTypes(tys(i), exprs(i)), p))
      case (t, p) => ExprHasType(p,t)
    }
  }

  override def toString: String = "(" + label + " " +  binder.toString + ")"

  override def printCoq(env: Environment): String = "(" + label.capitalize + " " + binder.printCoq(env) + ")"


}

case class ExpressionPattern(patternValue: Expression) extends Pattern {
  override def matches(value: Value, store: Store): Boolean = {
    patternValue.sameShapeAs(value)
  }

  override def substituteInto(expr: Expression, value: Value): Expression = {
    substituteInner(patternValue, expr, value)
  }

  private def substituteInner(patternValue: Expression, expr: Expression, value: Value): Expression = {
    patternValue match {
      case Num(_) | Bool(_) | UnitVal() => expr
      case Func(arg, ty, body) => ???
      case TupleValue(values) => subsTuple(values,expr, value)
      case TupleExpression(exprs) => subsTuple(exprs, expr, value)
      case Var(varName) => expr.substitute(varName, value)
      case VariantValue(label, value, _) => ???
    }
  }

  private def subsTuple(values: List[Expression],expr: Expression,  value:Value): Expression = {
    val valuesInTuple = value.ensureHasType[TupleValue]().values
    var outputExpr = expr
    values.indices.foreach(index => {
      outputExpr = substituteInner(values(index), outputExpr, valuesInTuple(index))
    })
    outputExpr
  }


  override def typeCheckBound(expr: Expression, env: Environment, valueType: Type): Type = {
    bindFreeVariables(patternValue, env, valueType)
    val exprTy = expr.typecheck(env)
    unbindFreeVariables(patternValue, env)
    exprTy
  }

  override def matchProp(value: Expression, env: Environment): Proposition = {
    ExprEq(patternValue, value)
  }



  override def toString: String = patternValue.toString()

  override def printCoq(env: Environment): String = patternValue.printCoq(env)

}


