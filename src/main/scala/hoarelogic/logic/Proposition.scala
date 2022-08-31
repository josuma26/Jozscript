package hoarelogic.logic

import language.Type
import language.expressions.Expression

trait Proposition {

  def quantify(): Proposition = {
    val (hasTypes, withoutHasTypes) = this.divideHasTypes()
    hasTypes.foldRight(withoutHasTypes)({
      case (HasType(varName, ty), prop) => UniversalQuantifier(varName, ty, prop)
    })
  }

  def divideHasTypes(): (List[HasType], Proposition)

  def substitute(name: String, e: Expression): Proposition
}

case class True() extends Proposition {
  override def substitute(name: String, e: Expression): Proposition = this

  override def divideHasTypes(): (List[HasType], Proposition) = {
    (List(), this)
  }

  override def toString: String = "True"
}
case class False() extends Proposition {
  override def substitute(name: String, e: Expression): Proposition = this

  override def divideHasTypes(): (List[HasType], Proposition) = {
    (List(), this)
  }

  override def toString: String = "False"
}

case class Implies(p: Proposition, q: Proposition) extends Proposition {
  override def substitute(name: String, e: Expression): Proposition =
    Implies(p.substitute(name, e), q.substitute(name, e))

  override def divideHasTypes(): (List[HasType], Proposition) = {
    val (lhtypes, lprop) = p.divideHasTypes()
    val (rhtypes, rprop) = q.divideHasTypes()
    (lhtypes ++ rhtypes, Implies(lprop, rprop))
  }

  override def toString: String = "(" + p.toString + ") -> (" + q.toString + ")"
}


case class And(p: Proposition, q: Proposition) extends Proposition {
  override def substitute(name: String, e: Expression): Proposition =
    And(p.substitute(name, e), q.substitute(name, e))

  override def divideHasTypes(): (List[HasType], Proposition) = {
    val (lhtypes, lprop) = p.divideHasTypes()
    val (rhtypes, rprop) = q.divideHasTypes()
    (lhtypes ++ rhtypes, And(lprop, rprop))
  }

  override def toString: String =  "(" + p.toString + ") /\\ (" + q.toString  + ")"
}

case class Or(p: Proposition, q: Proposition) extends Proposition {
  override def substitute(name: String, e: Expression): Proposition =
    Or(p.substitute(name, e), q.substitute(name, e))

  override def divideHasTypes(): (List[HasType], Proposition) = {
    val (lhtypes, lprop) = p.divideHasTypes()
    val (rhtypes, rprop) = q.divideHasTypes()
    (lhtypes ++ rhtypes, Or(lprop, rprop))
  }

  override def toString: String =  "(" + p.toString + ") \\/ (" + q.toString  + ")"
}

case class Not(p: Proposition) extends Proposition {
  override def substitute(name: String, e: Expression): Proposition = Not(p.substitute(name, e))

  override def divideHasTypes(): (List[HasType], Proposition) = {
    val (htypes, prop) = p.divideHasTypes()
    (htypes, Not(prop))
  }

  override def toString: String = "~" + p.toString
}

case class UniversalQuantifier(binder: String, ty: Type, prop: Proposition) extends Proposition {
  override def substitute(name: String, e: Expression): Proposition = {
    if (name.equals(binder)) this else UniversalQuantifier(binder, ty, prop.substitute(name, e))
  }

  override def divideHasTypes(): (List[HasType], Proposition) = {
    val (htypes, p) = prop.divideHasTypes()
    (htypes, UniversalQuantifier(binder, ty, p))
  }

  override def toString: String = "forall (" + binder + ": " + ty.printCoq() + "), " + prop.toString
}

case class ExprProp(expr: Expression) extends Proposition {
  override def substitute(name: String, v: Expression): Proposition = {
    ExprProp(expr.replace(name, v))
  }

  override  def divideHasTypes(): (List[HasType], Proposition) = (List(), this)

  override def toString: String = expr.printCoq()
}

case class VarEq(varName: String, expr: Expression) extends Proposition {
  override def substitute(name: String, e: Expression): Proposition = {
    if (name.equals(varName)) ExprEq(e, expr) else VarEq(varName, expr.replace(name, e))
  }

  override def divideHasTypes(): (List[HasType], Proposition) = (List(), this)


  override def toString: String = varName + " = " + expr.printCoq()
}

case class ExprEq(e1: Expression, e2: Expression) extends Proposition {
  override def substitute(name: String, e: Expression): Proposition = {
    ExprEq(e1.replace(name, e), e2.replace(name, e))
  }

  override def divideHasTypes(): (List[HasType], Proposition) = (List(), this)

  override def toString: String = e1.printCoq() + " = " + e2.printCoq()
}

case class HasType(varName: String, ty: Type) extends Proposition {
  override def substitute(name: String, e: Expression): Proposition = {
    if (varName.equals(name)) ExprHasType(e, ty) else this
  }

  override def divideHasTypes(): (List[HasType], Proposition) = {
    (List(this), True())
  }


  override def toString: String = varName + ": " + ty.printCoq()

}

case class ExprHasType(expr: Expression, ty: Type) extends Proposition {
  override def substitute(name: String, e: Expression): Proposition = {
    ExprHasType(expr.replace(name, e), ty)
  }

  override def divideHasTypes(): (List[HasType], Proposition) = {
    (List(), this)
  }
}
