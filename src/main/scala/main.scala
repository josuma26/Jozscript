
import language.expressions.{Expression, PatternMatch, Var}
import language.values.Value
import language.{Environment, ExpressionPattern, FunctionDefinition, LabelBinderPattern, NatType, ProductTy, Store, Type, TypeAlias, UnitType, expressions}
import preprocessing.Parser

object main {
  def main(args: Array[String]): Unit = {
    fromFile("src/main/scala/programs/trees.txt")
    //fromFile("src/main/scala/programs/lists.txt")
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
