package language

sealed trait Type {
  def ensureIsType[T <: Type]: T = {
    this match {
      case ty: T => ty
      case _ => throw new IllegalArgumentException("")
    }
  }
}

case class BoolType() extends Type {
  override def toString: String = "Bool"
}
case class NatType() extends Type {
  override def toString: String = "Nat"
}
case class UnitType() extends Type {
  override def toString: String = "Unit"
}

case class FuncTy(argTy: Type, retType: Type) extends Type {
  override def toString: String = "(" + argTy.toString + " -> " + retType.toString + ")"
}

case class ProductTy(types: List[Type]) extends Type {
  override def toString: String = types.map(_.toString).mkString("[", " * ","]")
}

case class SumTy(types: Map[String, Type]) extends Type {
  override def toString: String = types.map(pair => pair._1 + ": " + pair._2.toString).mkString("{",",","}")
}

/**
 * User defined type.
 *
 * type Name<T> := {
 *  | cons1: Name
 *  | cons2: T -> Name
 *  | ....
 *  |...
 *  }
 * @param name
 */

trait RecursiveType extends Type
case class BaseRecursiveType(name: String) extends RecursiveType
case class CustomType(name: String, constructors: Map[String, Type], typeVars: Map[String, Type])
  extends RecursiveType

object typeExpr {
  val natList = CustomType("natList",
    Map("nil" -> BaseRecursiveType("natList"), "cons" -> FuncTy(NatType(), BaseRecursiveType("natList"))),
    Map())
}

