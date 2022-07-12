package language

sealed trait Type {
  def ensureIsType[T <: Type]: T = {
    this match {
      case ty: T => ty
      case _ => throw new IllegalArgumentException("")
    }
  }
}

case class BoolType() extends Type
case class NatType() extends Type
case class UnitType() extends Type

case class FuncTy(argTy: Type, retType: Type) extends Type

case class ProductTy(types: List[Type]) extends Type

case class SumTy(types: Map[String, Type]) extends Type

