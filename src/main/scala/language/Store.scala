package language

import language.values.Value

class Store {
  private val values = collection.mutable.Map[String, Value]()
  
  def save(name: String, value: Value) = values.put(name, value)
  
  def lookup(name: String): Value = {
    val valueOpt = values.get(name)
    if (valueOpt.isEmpty) {
      throw new IllegalArgumentException("Value not found for name: " + name)
    }
    valueOpt.get
  }
}
