package language

/**
 * Expressions in the language.
 * x | \x:T, e | e e | e +-*% e | e &&|| e
 *
 */
trait Expression {

  def evaluate(store: Store): Value

  def substitute(variable: String, value: Value): Expression

  def typecheck(env: Environment): Type

  def ensureHasType[T <: Expression](): T = {
    this match {
      case e: T => e
      case _ => throw new IllegalStateException(s"Expected $this to be of a different tyoe.")
    }
  }

}

/**
 * A variable.
 *
 * Gamma(x)= T
 * -----------
 * Gamma |- x : Tau
 *
 */
case class Var(variable: String) extends Expression {
  override def evaluate(store: Store): Value = store.lookup(variable)

  override def typecheck(env: Environment): Type = env.get(variable)

  override def substitute(variable: String, value: Value): Expression = {
    if (this.variable.equals(variable)) value else this
  }
}

/**
 * Function application.
 *
 * Gamma |- e1 : Tau1 -> Tau2     Gamma |- e2 : Tau1
 * -------------------------------------------------
 *           Gamma |- e1 e2 : Tau2
 *
 */
case class App(e1: Expression, e2: Expression) extends Expression {
  override def evaluate(store: Store): Value = {
    val func = e1.evaluate(store).ensureHasType[Func]()
    val v2 = e2.evaluate(store)
    func.body.substitute(func.arg, v2).evaluate(store)
  }

  override def typecheck(env: Environment): Type = {
    val funcType = e1.typecheck(env).ensureIsType[FuncTy]
    val argType = e2.typecheck(env)
    if (funcType.argTy == argType) {
      return funcType.retType
    }
    throw new IllegalArgumentException(s"Expected argument $e2 to have type ${funcType.argTy}")
  }

  override def substitute(variable: String, value: Value): Expression = {
    App(e1.substitute(variable, value), e2.substitute(variable, value))
  }
}

/**
 * Abstract binary operation, can specify the input and return type.
 */
abstract class BinOp[T,R,  V <: Value, V2 <: Value](op: String, e1: Expression, e2: Expression, argTy: Type, retTy: Type) extends Expression {

  protected val opMap: Map[String, (T, T) => R]

  override def evaluate(store: Store): Value = {
    val left = e1.evaluate(store).ensureHasType[V]()
    val right = e2.evaluate(store).ensureHasType[V]()
    val func = operator(op)
    convertFrom(func(convertTo(left), convertTo(right)))
  }

  override def typecheck(env: Environment): Type = {
    val leftTy = e1.typecheck(env)
    val rightTy = e2.typecheck(env)
    if (leftTy == rightTy && rightTy == argTy) {
      return retTy
    }
    throw new IllegalArgumentException(s"Expected binary operation to have two $argTy arguments.")
  }

   def operator(op: String): (T, T) => R = {
     opMap.getOrElse(op, throw new IllegalArgumentException(s"Invalid operator $op"))
   }

   def convertTo(value: V): T

  def convertFrom(v: R): V2

}


case class NumBinOp(op: String,e1: Expression,  e2: Expression) extends
  BinOp[Int, Int, Num, Num](op, e1, e2, NatType(), NatType()) {
  override protected val opMap: Map[String, (Int, Int) => Int] = Map("+" -> (_+_), "-" -> (_-_),
  "*" -> (_*_),  "/" -> (_/_), "%" -> (_%_))

  override def convertTo(value: Num): Int = value.value

  override def convertFrom(v: Int): Num = Num(v)

  override def substitute(variable: String, value: Value): Expression = {
    NumBinOp(op, e1.substitute(variable, value), e2.substitute(variable, value))
  }
}


case class BoolBinOp(op: String, e1: Expression, e2: Expression) extends
  BinOp[Boolean, Boolean, Bool, Bool](op, e1, e2, BoolType(), BoolType()) {
  override def convertTo(value: Bool): Boolean = value.value

  override def convertFrom(v: Boolean): Bool = Bool(v)

  override def substitute(variable: String, value: Value): Expression = {
    BoolBinOp(op, e1.substitute(variable, value), e2.substitute(variable, value))
  }

  override protected val opMap: Map[String, (Boolean, Boolean) => Boolean] =
    Map("&&" -> (_ && _),
    "||" -> (_ || _))
}

case class NumCompOP(op: String, e1: Expression, e2:Expression) extends
  BinOp[Int, Boolean, Num, Bool](op, e1, e2, NatType(), BoolType()) {
  override protected val opMap: Map[String, (Int, Int) => Boolean] =
    Map("<" -> (_<_), ">" -> (_>_), "<=" -> (_<=_), ">=" -> (_>=_), "=" -> (_==_), "!=" -> (_!=_))
  override def convertTo(value: Num): Int = value.value

  override def convertFrom(v: Boolean): Bool = Bool(v)

  override def substitute(variable: String, value: Value): Expression = {
    NumCompOP(op, e1.substitute(variable, value), e2.substitute(variable, value))
  }
}

/**
 * A tuple projection.
 *
 * Gamma |- e : Tau1 * Tau 2 * ... * Tau n      1 <= i <= n
 * ----------------------------------------------------------
 *                 Gamma |- e.i : Tau i
 *
 */
case class Projection(e: Expression, index: Int) extends Expression {
  override def evaluate(store: Store): Value = {
    val tuple = e.evaluate(store).ensureHasType[Tuple]()
    tuple.values(index).evaluate(store)
  }

  override def substitute(variable: String, value: Value): Expression = Projection(e.substitute(variable, value), index)

  override def typecheck(env: Environment): Type = {
    e.typecheck(env) match {
      case ProductTy(types) => types(index)
      case _ => throw new IllegalArgumentException("Expected product type in projection.")
    }
  }
}

/**
 * Gamma |- e : {l1:Tau1, l2: Tau2, ..., ln: Taun}
 * foreach i Gamma;x_i:Tau_i |- t_i : Tau
 * -----------------------------------------------
 *  Gamma |- match e with
 *      case l1(x1) => t1
 *      ...
 *      case ln(xn) => tn
 *  : Tau
 */
case class Match(expr: Expression, cases: Map[String, (String, Expression)]) extends Expression {
  override def evaluate(store: Store): Value = {
    val variant = expr.evaluate(store).ensureHasType[Variant]()
    val pair = cases(variant.label)
    pair._2.substitute(pair._1, variant.expr.evaluate(store)).evaluate(store)
  }

  override def substitute(variable: String, value: Value): Expression = {
    Match(expr.substitute(variable, value),
      cases.map(kv => (kv._1, (kv._2._1, kv._2._2.substitute(variable, value)))))
  }

  override def typecheck(env: Environment): Type = {
    val variantTy = expr.typecheck(env).ensureIsType[SumTy]
    var evaluatedType: Option[Type] = Option.empty
    for ((label, ty) <- variantTy.types) {
      if (!cases.contains(label)) {
        throw new IllegalArgumentException(s"$label not contained in variant cases.")
      }
      val (binder, e) = cases(label)
      env.bind(binder, ty)
      val caseType = e.typecheck(env)
      env.unbind(binder)

      evaluatedType match {
        case Some(evTy) => assert(evTy == caseType)
        case None => evaluatedType = Some(caseType)
      }

    }
    evaluatedType.getOrElse(UnitType())
  }
}


case class Sequence(s1: Expression, s2: Expression) extends Expression {
  override def evaluate(store: Store): Value = {
    s1.evaluate(store); s2.evaluate(store)
  }

  override def substitute(variable: String, value: Value): Expression = {
    Sequence(s1.substitute(variable, value), s2.substitute(variable, value))
  }

  override def typecheck(env: Environment): Type = {
    s1.typecheck(env)
    s2.typecheck(env)
  }
}