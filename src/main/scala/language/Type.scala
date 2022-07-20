package language

sealed trait Type {
  def ensureIsType[T <: Type]: T = {
    this match {
      case ty: T => ty
      case _ => throw new IllegalArgumentException("")
    }
  }

  def eq(other: Type, environment: Environment): Boolean = {
    val thisConv = this.getIfAlias(environment)
    val otherConv = other.getIfAlias(environment)

    if (thisConv.equals(otherConv)) {
      return true
    }

    if (thisConv.getClass.equals(otherConv.getClass)) {
      return thisConv.innerEqual(other.asInstanceOf[thisConv.type], environment)
    }
    false
  }

  protected def innerEqual(other: this.type, environment: Environment): Boolean

  def getIfAlias(environment: Environment): Type = {
    this match {
      case TypeAlias(name) => environment.getAlias(name)
      case _ => this
    }
  }

  def substitute(name: String, ty: Type): Type = this
}

case class BoolType() extends Type {
  override def toString: String = "Bool"

  override protected def innerEqual(other: BoolType.this.type, environment: Environment): Boolean = true
}
case class NatType() extends Type {
  override def toString: String = "Nat"

  override protected def innerEqual(other: NatType.this.type, environment: Environment): Boolean = true
}
case class UnitType() extends Type {
  override def toString: String = "Unit"

  override protected def innerEqual(other: UnitType.this.type, environment: Environment): Boolean = true
}

case class FuncTy(argTy: Type, retType: Type) extends Type {
  override def toString: String = "(" + argTy.toString + " -> " + retType.toString + ")"

  override protected def innerEqual(other: FuncTy.this.type, environment: Environment): Boolean = {
    argTy.eq(other.argTy, environment) && retType.eq(other.retType, environment)
  }

  override def substitute(name: String, ty: Type): Type = {
    FuncTy(argTy.substitute(name, ty), retType.substitute(name, ty))
  }
}

case class ProductTy(types: List[Type]) extends Type {
  override def toString: String = types.map(_.toString).mkString("[", " * ","]")

  override protected def innerEqual(other: ProductTy.this.type, environment: Environment): Boolean = {
    types.size == other.types.length &&
    types.indices.forall(index => types(index).eq(other.types(index), environment))
  }

  override def substitute(name: String, ty: Type): Type = {
    ProductTy(types.map(_.substitute(name, ty)))
  }
}

case class SumTy(types: Map[String, Type]) extends Type {
  override def toString: String = types.map(pair => pair._1 + ": " + pair._2.toString).mkString("{",",","}")

  override protected def innerEqual(other: SumTy.this.type, environment: Environment): Boolean = {
    types.keySet.equals(other.types.keySet) &&
      types.keySet.forall(label => types(label).eq(other.types(label), environment))
  }

  override def substitute(name: String, ty: Type): Type = {
    SumTy(types.map({
      case (label, t) => (label, t.substitute(name, ty))
    }))
  }
}

case class TypeAlias(varName: String) extends Type {
  override protected def innerEqual(other: TypeAlias.this.type, environment: Environment): Boolean = {
    this.eq(other, environment)
  }

  override def substitute(name: String, ty: Type): Type = {
    if (name.equals(varName)) ty else this
  }

  override def toString: String = varName
}

case class UniversalType(typeVar: String, ty: Type) extends Type {
  override protected def innerEqual(other: UniversalType.this.type, environment: Environment): Boolean = {
    ty.eq(other.ty, environment)
  }

  override def substitute(name: String, ty: Type): Type = {
    if (typeVar.equals(name)) this else UniversalType(typeVar, this.ty.substitute(name, ty))
  }

  override def toString: String = "forall " + typeVar + ", " + ty.toString
}


