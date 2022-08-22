
import hoarelogic.logic.Proposition
import language.expressions.Expression
import language.values.Value
import language.{Environment, Store, Type}
import preprocessing.JFileReader

object main {
  def main(args: Array[String]): Unit = {
    JFileReader.read("src/main/scala/programs/sorting.txt").generateCoqFile("sample.v")
    //fromFile("src/main/scala/programs/fact.txt")
  }

  private def fromFile(fileName: String): (Type, Value) = {
    val expr = JFileReader.read(fileName)
    println(expr)
    runProgram(expr)
  }

  private def proofObligtion(pre: Proposition, post: Proposition, program: Expression) = {
    println(program.proofObligation(pre, post))
  }

  private def runProgram(program: Expression): (Type, Value) = {
    val ty = program.typecheck(new Environment())
    println(ty)
    val value = program.evaluate(new Store())
    println(value)
    (ty, value)
  }
}
