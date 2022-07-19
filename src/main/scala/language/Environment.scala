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
    typeMap.put(name, ty.getIfAlias(this))
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


}
