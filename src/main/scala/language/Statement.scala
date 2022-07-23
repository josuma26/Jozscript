package language

import language.expressions.Expression
import language.values.{Bool, Func, TypeAbstraction, UnitVal, Value}

trait Statement extends Expression {

  override def substitute(variable: String, value: Value): Statement

  override def typeSubs(typeVar: String, ty: Type): Statement

  override protected def checkSub(other: Statement.this.type): Boolean = ???

}

case class Skip() extends Statement {
  override def typecheck(env: Environment): Type = UnitType()

  override def evaluate(store: Store): Value = UnitVal()

  override def substitute(variable: String, value: Value): Statement = this

  override def toString: String = "skip"

  override def typeSubs(typeVar: String, ty: Type): Statement = this
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

  override def typeSubs(typeVar: String, ty: Type): Statement = {
    Assign(varName, value.typeSubs(typeVar, ty))
  }
}

case class IfStatement(cond: Expression, e1: Statement, e2: Statement) extends Statement {

  override def typecheck(env: Environment): Type = {
    cond.typecheck(env).ensureIsType[BoolType](env)
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

  override def typeSubs(typeVar: String, ty: Type): Statement = {
    IfStatement(cond.typeSubs(typeVar, ty), e1.typeSubs(typeVar, ty), e2.typeSubs(typeVar, ty))
  }

  override def toString: String = "if (" + cond.toString + ") {\n\t" + e1.toString + "\n} else {\n\t" + e2.toString + "}"
}

case class WhileLoop(cond: Expression, body: Expression) extends Statement {
  override def substitute(variable: String, value: Value): Statement = {
    WhileLoop(cond.substitute(variable, value), body.substitute(variable,value))
  }

  override def typeSubs(typeVar: String, ty: Type): Statement = {
    WhileLoop(cond.typeSubs(typeVar, ty), body.typeSubs(typeVar, ty))
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
    cond.typecheck(env).ensureIsType[BoolType](env)
    body.typecheck(env)
    UnitType()
  }

  override def toString: String = "while (" + cond.toString + ") {\n\t" + body.toString + "}"
}

case class TypeDefinition(name: String, ty: Type) extends Statement {
  override def substitute(variable: String, value: Value): Statement = this

  override def typeSubs(typeVar: String, ty: Type): Statement = this

  override def evaluate(store: Store): Value = UnitVal()

  override def typecheck(env: Environment): Type = {
    env.saveAlias(name, ty)
    UnitType()
  }
}

case class FunctionDefinition(name: String, typeVars: List[String],
                              args:Map[String, Type], retTy: Type, body: Expression) extends Statement {
  override def substitute(variable: String, value: Value): Statement = {
    if (args.keySet.contains(variable)) {
      this
    } else {
      FunctionDefinition(name, typeVars, args, retTy,  body.substitute(variable, value))
    }
  }

  override def typeSubs(typeVar: String, ty: Type): Statement = {
    FunctionDefinition(name,
      typeVars,
      args.map({case (l, t) => (l, t.substitute(typeVar, ty))}),
      retTy.substitute(typeVar, ty),
      body.typeSubs(typeVar, ty))
  }

  override def evaluate(store: Store): Value = {
    var curried = curry(args.keySet.toList.reverse, body)
    typeVars.foreach(typeVar => curried = TypeAbstraction(typeVar, curried))
    store.save(name, curried.evaluate(store))
    UnitVal()
  }


  override def typecheck(env: Environment): Type = {
    var curriedType = curriedArguments(args.keySet.toList.reverse, retTy)
    var curried = curry(args.keySet.toList.reverse, body)

    typeVars.foreach(typeVar => {
      curriedType = UniversalType(typeVar, curriedType)
      curried = TypeAbstraction(typeVar, curried)
    })

    env.bind(name, curriedType)
    curried.typecheck(env)
    UnitType()
  }

  private def curry(args: List[String], acc: Expression): Expression = {
    if (args.isEmpty) {
      acc
    } else {
      val (_,rest) = args.splitAt(1)
      val arg = args.head
      curry(rest, Func(arg, this.args(arg), acc))
    }
  }

  private def curriedArguments(args: List[String], acc: Type): Type = {
    if (args.isEmpty) {
      acc
    } else {
      val (_, rest) = args.splitAt(1)
      val arg = args.head
      curriedArguments(rest, FuncTy(this.args(arg), acc))
    }
  }
}



