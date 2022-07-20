package language

import language.values.Value

class Store {
  private val values = collection.mutable.Map[String, Value]()

  private val typeSubstitution = collection.mutable.Map[String, Type]()
  
  def save(name: String, value: Value) = values.put(name, value)
  
  def lookup(name: String): Value = {
    val valueOpt = values.get(name)
    if (valueOpt.isEmpty) {
      throw new IllegalArgumentException("Value not found for name: " + name)
    }
    valueOpt.get
  }

  def saveType(name: String, ty: Type) = typeSubstitution.put(name, ty)

  def lookupType(name: String): Type = {
    if (typeSubstitution.contains(name)) {
      return typeSubstitution(name)
    }
    throw new IllegalArgumentException(s"No type for type variable: $name")
  }
}
