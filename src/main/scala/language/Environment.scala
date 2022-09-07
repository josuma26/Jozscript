package language

class Environment {
  private val typeMap = collection.mutable.Map[String, Type]()

  private val typeAliases = collection.mutable.Map[String, Type]()

  def get(name: String): Type = {
    val typeOpt = typeMap.get(name)
    if (typeOpt.isEmpty) {
      throw new IllegalArgumentException(s"No type in environment for: $name")
    }
    typeOpt.get
  }

  def bind(name: String, ty: Type): Unit = {
    if (typeMap.contains(name) && !typeMap(name).eq(ty, this)) {
      throw new IllegalArgumentException(s"Can not change a variables type in the environment. " +
        s"$name has type ${typeMap(name)}, can not bind to $ty.")
    }
    typeMap.put(name, ty.getIfAlias(this))
  }

  def bindIfFree(name: String, ty: Type): Unit = {
    if (!typeMap.contains(name)) {
      typeMap.put(name, ty.getIfAlias(this))
    }
  }

  def unbind(name: String): Unit = {
    typeMap.remove(name)
  }

  def saveAlias(name: String, ty: Type): Unit = {
    if (typeAliases.contains(name)) {
      throw new IllegalArgumentException(s"Type $name is alreay defined.")
    }
    typeAliases.put(name, ty)
  }

  def getAlias(name: String): Type = {
    if (typeAliases.contains(name)) {
      return typeAliases(name)
    }
    throw new IllegalArgumentException(s"No type alias for name: $name")
  }

  def getAliasIfBound(name: String): Type = {
    if (typeAliases.contains(name)) {
      return typeAliases(name)
    }
    TypeAlias(name)
  }

  def isAlias(name: String): Boolean = typeAliases.contains(name)

  def deleteAlias(name: String): Unit = {
    if (!typeAliases.contains(name)) {
      throw new IllegalArgumentException(s"No type alias for name: $name. Cannot delete")
    }
    typeAliases.remove(name)
  }


}
