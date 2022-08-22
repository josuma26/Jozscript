package language.expressions

import language._
import language.values.{Func, Value}

/**
 * Function application.
 *
 * Gamma |- e1 : Tau1 -> Tau2     Gamma |- e2 : Tau1
 * -------------------------------------------------
 * Gamma |- e1 e2 : Tau2
 *
 */
case class App(e1: Expression, e2: Expression) extends Expression {
  override def evaluate(store: Store): Value = {
    val func = e1.evaluate(store).ensureHasType[Func]()
    val v2 = e2.evaluate(store)
    func.body.substitute(func.arg, v2).evaluate(store)
  }

  override def typecheck(env: Environment): Type = {
    val funcType = e1.typecheck(env).ensureIsType[FuncTy](env)
    val argType = e2.typecheck(env)
    if (funcType.argTy.eq(argType, env)) {
      return funcType.retType
    }
    throw new IllegalArgumentException(s"Expected argument $e2 to have type ${funcType.argTy}. Found $argType")
  }

  override def substitute(variable: String, value: Value): Expression = {
    App(e1.substitute(variable, value), e2.substitute(variable, value))
  }

  override def replace(variable: String, value: Expression): Expression = {
    App(e1.replace(variable, value), e2.replace(variable, value))
  }

  override def typeSubs(typeVar: String, ty: Type): Expression = {
    App(e1.typeSubs(typeVar, ty), e2.typeSubs(typeVar, ty))
  }

  override def checkSub(other: App.this.type): Boolean = e1.sameShapeAs(other.e1) && e2.sameShapeAs(other.e2)

  override def toString: String = e1.toString + "(" + e2.toString + ")"

  override def printCoq(): String = "(" + e1.printCoq() + " " + e2.printCoq() + ")"
}
