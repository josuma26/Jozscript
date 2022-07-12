package language

trait Statement extends Expression {

  override def typecheck(env: Environment): Type = UnitType()

  override def substitute(variable: String, value: Value): Statement

}

case class Skip() extends Statement {
  override def evaluate(store: Store): Value = UnitVal()

  override def substitute(variable: String, value: Value): Statement = this
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
}

case class IfStatement(cond: Expression, e1: Statement, e2: Statement) extends Statement {
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
}



