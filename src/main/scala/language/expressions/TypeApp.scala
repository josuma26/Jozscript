package language.expressions

import language.values.{TypeAbstraction, Value}
import language.{Environment, Store, Type, UniversalType}

case class TypeApp(e: Expression, ty: Type) extends Expression {
  override def evaluate(store: Store): Value = {
    val asTypeAbs = e.evaluate(store).ensureHasType[TypeAbstraction]()
    asTypeAbs.body.typeSubs(asTypeAbs.typeArg, ty).evaluate(store)
  }

  override def substitute(variable: String, value: Value): Expression = {
    TypeApp(e.substitute(variable, value), ty)
  }

  override def typeSubs(typeVar: String, ty: Type): Expression = {
    TypeApp(e, this.ty.substitute(typeVar, ty))
  }

  override def typecheck(env: Environment): Type = {
    val exprTy = e.typecheck(env).ensureIsType[UniversalType]
    exprTy.ty.substitute(exprTy.typeVar, ty)
  }

  override protected def checkSub(other: TypeApp.this.type): Boolean = {
    this.e.sameShapeAs(other.e)
  }
}
