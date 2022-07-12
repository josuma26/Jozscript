package language

class Store {
  private val values = collection.mutable.Map[String, Value]()
  
  def save(name: String, value: Value) = {
    if (values.contains(name)) {
      throw new IllegalArgumentException("Value is already binded: " + name)
    }
    values.put(name, value)
  }
  
  def lookup(name: String): Value = {
    val valueOpt = values.get(name)
    if (valueOpt.isEmpty) {
      throw new IllegalArgumentException("Value not found for name: " + name)
    }
    valueOpt.get
  }


}
