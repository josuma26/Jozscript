
import language.{Environment, Expression, Store, Type, Value}
import preprocessing.Parser

object main {
  def main(args: Array[String]): Unit = {
    fromFile("src/main/scala/programs/firstprogram.txt")
  }

  private def fromFile(fileName: String): (Type, Value) = {
    val expr = FIleReader.read(fileName)
    println(expr)
    runProgram(expr)
  }

  private def runProgram(program: Expression): (Type, Value) = {
    val ty = program.typecheck(new Environment())
    println(ty)
    val value = program.evaluate(new Store())
    println(value)
    (ty, value)
  }
}
