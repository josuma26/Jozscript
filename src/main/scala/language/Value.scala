package language

sealed trait Value extends Expression {
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
}

case class Bool(value: Boolean) extends Value {
  override def typecheck(env: Environment): Type = BoolType()
}
case class Num(value: Int) extends Value {
  override def typecheck(env: Environment): Type = NatType()
}
case class UnitVal() extends Value {
  override def typecheck(env: Environment): Type = UnitType()
}

case class Tuple(values: List[Expression]) extends Value {
  override def evaluate(store: Store): Value = Tuple(values.map(e => e.evaluate(store)))

  override def substitute(variable: String, value: Value): Expression = {
    Tuple(values.map(e => e.substitute(variable, value)))
  }

  override def typecheck(env: Environment): Type = ProductTy(values.map(v => v.typecheck(env)))
}

case class Variant(label: String, expr: Expression, ty: Type) extends Value {
  override def typecheck(env: Environment): Type = {
    val optTy = ty.ensureIsType[SumTy].types.get(label)
    if (optTy.isEmpty) {
      throw new IllegalArgumentException(s"$label not a type in $ty")
    }
    val variantBodyType = expr.typecheck(env)
    if (variantBodyType != optTy.get) {
      throw new IllegalArgumentException(s"Expected variant body to have ${optTy.get}, found $variantBodyType")
    }
    ty
  }

  override def substitute(variable: String, value: Value): Expression = {
    Variant(label, expr.substitute(variable, value), ty)
  }
}


