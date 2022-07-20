package language.expressions

import language.values.{Bool, Num, Value}
import language.{BoolType, Environment, NatType, Store, Type}

/**
 * Abstract binary operation, can specify the input and return type.
 */
abstract class BinOp[T, R, V <: Value, V2 <: Value](op: String, e1: Expression, e2: Expression, argTy: Type, retTy: Type) extends Expression {

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
    if (leftTy.eq(rightTy, env) && rightTy.eq(argTy, env)) {
      return retTy
    }
    throw new IllegalArgumentException(s"Expected binary operation to have two $argTy arguments.")
  }

  def operator(op: String): (T, T) => R = {
    opMap.getOrElse(op, throw new IllegalArgumentException(s"Invalid operator $op"))
  }

  def convertTo(value: V): T

  def convertFrom(v: R): V2

  override def toString: String = e1.toString + " " + op + " " + e2.toString
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

  override def typeSubs(typeVar: String, ty: Type): Expression = {
    NumBinOp(op, e1.typeSubs(typeVar, ty), e2.typeSubs(typeVar, ty))
  }

  override protected def checkSub(other: NumBinOp.this.type): Boolean = {
    op.equals(other.op) && e1.sameShapeAs(other.e1) && e2.sameShapeAs(other.e2)
  }

}


case class BoolBinOp(op: String, e1: Expression, e2: Expression) extends
  BinOp[Boolean, Boolean, Bool, Bool](op, e1, e2, BoolType(), BoolType()) {
  override def convertTo(value: Bool): Boolean = value.value

  override def convertFrom(v: Boolean): Bool = Bool(v)

  override def substitute(variable: String, value: Value): Expression = {
    BoolBinOp(op, e1.substitute(variable, value), e2.substitute(variable, value))
  }

  override def typeSubs(typeVar: String, ty: Type): Expression = {
    BoolBinOp(op, e1.typeSubs(typeVar, ty), e2.typeSubs(typeVar, ty))
  }

  override protected def checkSub(other: BoolBinOp.this.type): Boolean = {
    op.equals(other.op) && e1.sameShapeAs(other.e1) && e2.sameShapeAs(other.e2)
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

  override def typeSubs(typeVar: String, ty: Type): Expression = {
    NumCompOP(op, e1.typeSubs(typeVar, ty), e2.typeSubs(typeVar, ty))
  }

  override protected def checkSub(other: NumCompOP.this.type): Boolean = {
    op.equals(other.op) && e1.sameShapeAs(other.e1) && e2.sameShapeAs(other.e2)
  }
}
