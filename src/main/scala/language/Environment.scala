package language

class Environment {
  private val typeMap = collection.mutable.Map[String, Type]()

  def get(name: String): Type = {
    val typeOpt = typeMap.get(name)
    if (typeOpt.isEmpty) {
      throw new IllegalArgumentException(s"No type in environment for: $name")
    }
    typeOpt.get
  }

  def bind(name: String, ty: Type): Unit = {
    typeMap.put(name, ty)
  }

  def unbind(name: String): Unit = {
    typeMap.remove(name)
  }


}
