package language

import hoarelogic.logic._
import language.expressions.{Expression, Var}
import language.values._
import preprocessing.JFileReader


trait Statement extends Expression {

  override def substitute(variable: String, value: Value): Statement

  override def typeSubs(typeVar: String, ty: Type): Statement

  override protected def checkSub(other: Statement.this.type): Boolean = ???

}

case class Skip() extends Statement {
  override def typecheck(env: Environment): Type = UnitType()

  override def evaluate(store: Store): Value = UnitVal()

  override def substitute(variable: String, value: Value): Statement = this

  override def replace(variable: String, expr: Expression): Expression = this

  override def toString: String = "skip"

  override def typeSubs(typeVar: String, ty: Type): Statement = this
}

case class Assign(varName: String, value: Expression) extends Statement {

  override def proofObligation(pre: Proposition, post: Proposition): Proposition = {
    Implies(pre, post.substitute(varName, value))
  }

  override def pushThrough(pre: Proposition): Proposition = {
    val newVar = Var(varName + "'")
    And(pre.substitute(varName, newVar), VarEq(varName, value.replace(varName, newVar)))
  }

  override def typecheck(env: Environment): Type = {
    env.bind(varName, value.typecheck(env))
    UnitType()
  }

  override def evaluate(store: Store): Value = {
    store.save(varName, value.evaluate(store))
    UnitVal()
  }

  override def substitute(variable: String, value: Value): Statement = {
    Assign(varName, this.value.substitute(variable, value))
  }

  override def replace(variable: String, expr: Expression): Expression = {
    Assign(varName, value.replace(variable, expr))
  }

  override def toString: String = "let " + varName + " := " + value.toString

  override def printCoq(): String = "Definition " + varName + " := " + value.printCoq() + "."

  override def typeSubs(typeVar: String, ty: Type): Statement = {
    Assign(varName, value.typeSubs(typeVar, ty))
  }
}

case class IfStatement(cond: Expression, e1: Expression, e2: Expression) extends Statement {

  override def proofObligation(pre: Proposition, post: Proposition): Proposition = {
    val e1Holds = e1.proofObligation(And(pre, ExprProp(cond)), post)
    val e2Holds = e2.proofObligation(And(pre, Not(ExprProp(cond))), post)
    And(e1Holds, e2Holds)
  }
  override def typecheck(env: Environment): Type = {
    cond.typecheck(env).ensureIsType[BoolType](env)
    e1.typecheck(env)
    e2.typecheck(env)
    UnitType()
  }

  override def evaluate(store: Store): Value = {
    val condVal = cond.evaluate(store).ensureHasType[Bool]().value
    if (condVal) {
      e1.evaluate(store)
    } else {
      e2.evaluate(store)
    }
    UnitVal()
  }

  override def substitute(variable: String, value: Value): Statement = {
    IfStatement(cond.substitute(variable, value), e1.substitute(variable, value), e2.substitute(variable, value))
  }

  override def replace(variable: String, value: Expression): Statement = {
    IfStatement(cond.replace(variable, value), e1.replace(variable, value), e2.replace(variable, value))
  }

  override def typeSubs(typeVar: String, ty: Type): Statement = {
    IfStatement(cond.typeSubs(typeVar, ty), e1.typeSubs(typeVar, ty), e2.typeSubs(typeVar, ty))
  }

  override def toString: String = "if (" + cond.toString + ") {\n\t" + e1.toString + "\n} else {\n\t" + e2.toString + "}"
}

case class WhileLoop(cond: Expression, body: Expression, invariant: Proposition= True()) extends Statement {

  override def proofObligation(pre: Proposition, post: Proposition): Proposition = {
    val invHoldsStart = Implies(pre, invariant)
    val invPreserved = body.proofObligation(And(invariant, ExprProp(cond)), invariant)
    val impPost = Implies(And(invariant, Not(ExprProp(cond))), post)
    And(And(invHoldsStart, invPreserved), impPost)
  }

  override def pushThrough(pre: Proposition): Proposition = {
    And(invariant, Not(ExprProp(cond)))
  }

  override def impliesPushed(pre: Proposition): Proposition = {
    proofObligation(pre, pushThrough(pre))
  }

  override def substitute(variable: String, value: Value): Statement = {
    WhileLoop(cond.substitute(variable, value), body.substitute(variable,value))
  }

  override def replace(variable: String, value: Expression): Statement = {
    WhileLoop(cond.replace(variable, value), body.replace(variable,value))
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

  override def proofObligation(pre: Proposition, post: Proposition): Proposition = True()

  override def substitute(variable: String, value: Value): Statement = this

  override def replace(variable: String, expr: Expression): Expression = this

  override def typeSubs(typeVar: String, ty: Type): Statement = this

  override def evaluate(store: Store): Value = UnitVal()

  override def typecheck(env: Environment): Type = {
    env.saveAlias(name, ty)
    UnitType()
  }

  override def printCoq(): String = "" //Inductive " + name + " := \n" + ty.printCoq() + "."
}

case class FunctionDefinition(name: String, typeVars: List[String],
                              args:Map[String, Type], retTy: Type, body: Expression) extends Statement {

  override def proofObligation(pre: Proposition, post: Proposition): Proposition = {
    var preWithAllTypes = pre
    args.foreach(p => {
      preWithAllTypes = And(HasType(p._1, p._2), preWithAllTypes)
    })
    body.proofObligation(preWithAllTypes, post)
  }

  override def substitute(variable: String, value: Value): Statement = {
    if (args.keySet.contains(variable)) {
      this
    } else {
      FunctionDefinition(name, typeVars, args, retTy,  body.substitute(variable, value))
    }
  }

  override def replace(variable: String, value: Expression): Statement = {
    if (args.keySet.contains(variable)) {
      this
    } else {
      FunctionDefinition(name, typeVars, args, retTy,  body.replace(variable, value))
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

  override def printCoq(): String = {
    val typeArgs = if (typeVars.isEmpty)  "" else typeVars.mkString(" {", " ","}")
    val argsString = args.map({
      case (label, ty) => " (" + label + ": " + ty.printCoq() + ")"
    }).mkString(" ")
    "Fixpoint " + name + typeArgs + argsString + ": " + retTy.printCoq() + " := \n\t" + body.printCoq() + "."
  }
}

case class ImportStatement(packageName: String) extends Statement {

  override def substitute(variable: String, value: Value): Statement = this

  override def typeSubs(typeVar: String, ty: Type): Statement = this

  override def evaluate(store: Store): Value = read().evaluate(store)

  override def replace(variable: String, expr: Expression): Expression = this

  override def typecheck(env: Environment): Type = read().typecheck(env)

  private def read(): Expression = {
    val fileName = "src/main/scala/programs/" + packageName + ".txt"
    JFileReader.read(fileName)
  }

  override def toString: String = "import " + packageName

  override def printCoq(): String = read().printCoq()
}



