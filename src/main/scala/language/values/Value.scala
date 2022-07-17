package language.values

import language._
import language.expressions.{Expression, TupleExpression, VariantExpression}

trait Value extends Expression {
  override def evaluate(store: Store): Value = this

  override def substitute(variable: String, value: Value): Expression = this

}

case class Func(arg: String, ty: Type, body: Expression) extends Value {
  override def substitute(variable: String, value: Value): Expression = {
    if (!arg.equals(variable)) {
      Func(arg, ty, body.substitute(variable, value))
    } else {
      this
    }
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

  override def typecheck(env: Environment): Type = ProductTy(values.map(v => v.typecheck(env)))

  override def toString: String = values.map(_.toString).mkString("[", ", ","]")

  override protected def checkSub(other: TupleValue.this.type): Boolean = {
    values.length == other.values.length && values.indices.forall(index => values(index).sameShapeAs(other.values(index)))
  }
}


case class VariantValue(label: String, value: Value, ty: Type) extends Value {
  override def typecheck(env: Environment): Type = {
    val optTy = ty.ensureIsType[SumTy].types.get(label)
    if (optTy.isEmpty) {
      throw new IllegalArgumentException(s"$label not a type in $ty")
    }
    val variantBodyType = value.typecheck(env)
    if (variantBodyType != optTy.get) {
      throw new IllegalArgumentException(s"Expected variant body to have ${optTy.get}, found $variantBodyType")
    }
    ty
  }

  override protected def checkSub(other: VariantValue.this.type): Boolean = {
    label.equals(other.label) && value.sameShapeAs(other.value) && ty.equals(other.ty)
  }
}


