package language

import language.expressions.Expression
import language.values.{Bool, UnitVal, Value}

trait Statement extends Expression {

  override def substitute(variable: String, value: Value): Statement

  override protected def checkSub(other: Statement.this.type): Boolean = ???

}

case class Skip() extends Statement {
  override def typecheck(env: Environment): Type = UnitType()

  override def evaluate(store: Store): Value = UnitVal()

  override def substitute(variable: String, value: Value): Statement = this

  override def toString: String = "skip"
}

case class Assign(varName: String, value: Expression) extends Statement {

  override def typecheck(env: Environment): Type = {
    env.bind(varName, value.typecheck(env))
    UnitType()
  }

  override def evaluate(store: Store): Value = {
    store.save(varName, value.evaluate(store))
    UnitVal()
  }

  override def substitute(variable: String, value: Value): Statement = {
    Assign(varName, value.substitute(variable, value))
  }

  override def toString: String = "let " + varName + " := " + value.toString
}

case class IfStatement(cond: Expression, e1: Statement, e2: Statement) extends Statement {

  override def typecheck(env: Environment): Type = {
    cond.typecheck(env).ensureIsType[BoolType]
    e1.typecheck(env)
    e2.typecheck(env)
  }

  override def evaluate(store: Store): Value = {
    val condVal = cond.evaluate(store).ensureHasType[Bool]().value
    if (condVal) {
      e1.evaluate(store)
    } else {
      e2.evaluate(store)
    }
  }

  override def substitute(variable: String, value: Value): Statement = {
    IfStatement(cond.substitute(variable, value), e1.substitute(variable, value), e2.substitute(variable, value))
  }

  override def toString: String = "if (" + cond.toString + ") {\n\t" + e1.toString + "\n} else {\n\t" + e2.toString + "}"
}

case class WhileLoop(cond: Expression, body: Expression) extends Statement {
  override def substitute(variable: String, value: Value): Statement = {
    WhileLoop(cond.substitute(variable, value), body.substitute(variable,value))
  }

  override def evaluate(store: Store): Value = {
    val condValue = cond.evaluate(store).ensureHasType[Bool]().value
    if (condValue) {
      body.evaluate(store)
      evaluate(store)
    }
    UnitVal()
  }

  override def typecheck(env: Environment): Type = {
    cond.typecheck(env).ensureIsType[BoolType]
    body.typecheck(env)
    UnitType()
  }

  override def toString: String = "while (" + cond.toString + ") {\n\t" + body.toString + "}"
}



