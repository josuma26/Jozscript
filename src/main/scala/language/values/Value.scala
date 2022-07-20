package language.values

import language._
import language.expressions.{Expression, TupleExpression}

trait Value extends Expression {
  override def evaluate(store: Store): Value = this

  override def substitute(variable: String, value: Value): Value = this

  override def typeSubs(typeVar: String, ty: Type): Value = this

}

case class Func(arg: String, ty: Type, body: Expression) extends Value {
  override def substitute(variable: String, value: Value): Value = {
    if (!arg.equals(variable)) {
      Func(arg, ty, body.substitute(variable, value))
    } else {
      this
    }
  }

  override def typeSubs(typeVar: String, ty: Type): Value = {
    Func(arg, this.ty.substitute(typeVar, ty), body.typeSubs(typeVar, ty))
  }

  override def typecheck(env: Environment): Type = {
    env.bind(arg, ty)
    val retTy = body.typecheck(env)
    env.unbind(arg)
    FuncTy(ty, retTy)
  }

  override def checkSub(other: Func.this.type): Boolean =
    ty.equals(other.ty) && body.sameShapeAs(other.body)

  override def toString: String = "lambda " + arg + ": " + ty.toString + ", " + body.toString
}

case class Bool(value: Boolean) extends Value {
  override def typecheck(env: Environment): Type = BoolType()

  override def toString: String = value.toString

  override protected def checkSub(other: Bool.this.type): Boolean = value.equals(other.value)
}

case class Num(value: Int) extends Value {
  override def typecheck(env: Environment): Type = NatType()

  override def toString: String = value.toString

  override protected def checkSub(other: Num.this.type): Boolean = value.equals(other.value)
}

case class UnitVal() extends Value {
  override def typecheck(env: Environment): Type = UnitType()

  override def toString: String = "unit"

  override protected def checkSub(other: UnitVal.this.type): Boolean = true
}


case class TupleValue(values: List[Value]) extends Value {

  override def substitute(variable: String, value: Value): Value = {
    TupleValue(values.map(_.substitute(variable, value)))
  }

  override def typeSubs(typeVar: String, ty: Type): Value = {
    TupleValue(values.map(_.typeSubs(typeVar, ty)))
  }

  override def typecheck(env: Environment): Type = ProductTy(values.map(v => v.typecheck(env)))

  override def toString: String = values.map(_.toString).mkString("[", ", ","]")

  override def sameShapeAs(other: Expression): Boolean = {
    other match {
      case TupleExpression(otherExprs) => checkExpressions(otherExprs)
      case TupleValue(otherExprs) => checkExpressions(otherExprs)
      case _ => false
    }
  }

  override protected def checkSub(other: TupleValue.this.type): Boolean = {
    checkExpressions(other.values)
  }

  private def checkExpressions(otherExprs: List[Expression]): Boolean = {
    values.length == otherExprs.length && values.indices.forall(index => values(index).sameShapeAs(otherExprs(index)))
  }
}


case class VariantValue(label: String, value: Value, ty: Type) extends Value {
  override def typecheck(env: Environment): Type = {
    val optTy = ty.getIfAlias(env).ensureIsType[SumTy].types.get(label)
    if (optTy.isEmpty) {
      throw new IllegalArgumentException(s"$label not a type in $ty")
    }
    val variantBodyType = value.typecheck(env)
    if (!variantBodyType.eq(optTy.get, env)) {
      throw new IllegalArgumentException(s"Expected variant body to have ${optTy.get}, found $variantBodyType")
    }
    ty
  }

  override def substitute(variable: String, value: Value): Value = {
    VariantValue(label, value.substitute(variable, value), ty)
  }

  override def typeSubs(typeVar: String, ty: Type): Value = {
    VariantValue(label, value.typeSubs(typeVar, ty),this.ty.substitute(typeVar, ty))
  }

  override protected def checkSub(other: VariantValue.this.type): Boolean = {
    label.equals(other.label) && value.sameShapeAs(other.value) && ty.equals(other.ty)
  }
}

case class TypeAbstraction(typeArg: String, body: Expression) extends Value {

  override def substitute(variable: String, value: Value): Value = {
    TypeAbstraction(typeArg, body.substitute(variable, value))
  }

  override def typeSubs(typeVar: String, ty: Type): Value = {
    if (typeArg.equals(typeVar)) {
      this
    } else {
      TypeAbstraction(typeArg, body.typeSubs(typeVar, ty))
    }
  }

  /**
   * To type-check a type abstraction, the body must type check with _typeArg_ having a well-formed type.
   */
  override def typecheck(env: Environment): Type = {
    env.saveAlias(typeArg, TypeAlias(typeArg))
    val bodyType = body.typecheck(env)
    env.deleteAlias(typeArg)
    UniversalType(typeArg, bodyType)
  }

  override protected def checkSub(other: TypeAbstraction.this.type): Boolean = {
    body.sameShapeAs(other.body)
  }
}




