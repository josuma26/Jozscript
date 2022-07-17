
import language.expressions.{Expression, PatternMatch, Var}
import language.values.Value
import language.{Environment, LabelBinderPattern, NatType, ProductTy, Store, Type, UnitType, ValuePattern, expressions}
import preprocessing.Parser

object main {
  def main(args: Array[String]): Unit = {

    val expr = Parser.parse("l:[1,2] as {l:[Nat*Nat], num:Nat}")
    val tuplePattern = ValuePattern(Parser.parse("[a,b]"))
    val lPattern = LabelBinderPattern("l", tuplePattern)
    val numPatter = LabelBinderPattern("num", ValuePattern(Parser.parse("n")))
    val lExpression = Parser.parse("[a,b]")
    val numExpression = Parser.parse("[n,0]")
    val patternMatch = PatternMatch(expr, List((lPattern, lExpression), (numPatter, numExpression)))
    println(patternMatch)
    println(patternMatch.typecheck(new Environment()))
    //fromFile("src/main/scala/programs/firstprogram.txt")
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
