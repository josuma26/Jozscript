package hoarelogic.logic

import language.Type
import language.expressions.Expression

trait Proposition {
  def substitute(name: String, e: Expression): Proposition
}

case class True() extends Proposition {
  override def substitute(name: String, e: Expression): Proposition = this

  override def toString: String = "True"
}
case class False() extends Proposition {
  override def substitute(name: String, e: Expression): Proposition = this

  override def toString: String = "False"
}

case class Implies(p: Proposition, q: Proposition) extends Proposition {
  override def substitute(name: String, e: Expression): Proposition =
    Implies(p.substitute(name, e), q.substitute(name, e))

  override def toString: String = "(" + p.toString + ") -> (" + q.toString + ")"
}


case class And(p: Proposition, q: Proposition) extends Proposition {
  override def substitute(name: String, e: Expression): Proposition =
    And(p.substitute(name, e), q.substitute(name, e))

  override def toString: String =  "(" + p.toString + ") /\\ (" + q.toString  + ")"
}

case class Or(p: Proposition, q: Proposition) extends Proposition {
  override def substitute(name: String, e: Expression): Proposition =
    Or(p.substitute(name, e), q.substitute(name, e))

  override def toString: String =  "(" + p.toString + ") \\/ (" + q.toString  + ")"
}

case class Not(p: Proposition) extends Proposition {
  override def substitute(name: String, e: Expression): Proposition = Not(p.substitute(name, e))

  override def toString: String = "~" + p.toString
}

case class ExprProp(expr: Expression) extends Proposition {
  override def substitute(name: String, v: Expression): Proposition = {
    ExprProp(expr.replace(name, v))
  }

  override def toString: String = expr.printCoq()
}

case class VarEq(varName: String, expr: Expression) extends Proposition {
  override def substitute(name: String, e: Expression): Proposition = {
    if (name.equals(varName)) ExprEq(e, expr) else VarEq(varName, expr.replace(name, e))
  }

  override def toString: String = varName + " = " + expr.printCoq()
}

case class ExprEq(e1: Expression, e2: Expression) extends Proposition {
  override def substitute(name: String, e: Expression): Proposition = {
    ExprEq(e1.replace(name, e), e2.replace(name, e))
  }

  override def toString: String = e1.printCoq() + " = " + e2.printCoq()
}

case class HasType(varName: String, ty: Type) extends Proposition {
  override def substitute(name: String, e: Expression): Proposition = {
    if (varName.equals(name)) ExprHasType(e, ty) else this
  }
}

case class ExprHasType(expr: Expression, ty: Type) extends Proposition {
  override def substitute(name: String, e: Expression): Proposition = {
    ExprHasType(expr.replace(name, e), ty)
  }
}
