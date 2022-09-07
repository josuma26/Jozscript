
import language.expressions.Expression
import language.values.Value
import language.{Environment, Store, Type}
import preprocessing.JFileReader

object main {

  private val PROGRAM_PATH = "src/main/scala/programs/"
  private val OUTPUT_PATH = "src/main/proofs/"

  def main(args: Array[String]): Unit = {
    verifyProgram("graphs")
  }

  private def verifyProgram(progName: String) = {
    val programFile = PROGRAM_PATH + progName + ".txt"
    val outputFile = OUTPUT_PATH + progName + "_proof.v"
    val (_,_, env) = executeFromFile(progName)
    JFileReader.read(programFile).generateCoqFile(outputFile, env)
  }

  private def executeFromFile(progName: String): (Type, Value, Environment) = {
    val expr = JFileReader.read(PROGRAM_PATH + progName + ".txt")
    println(expr)
    runProgram(expr)
  }

  private def runProgram(program: Expression): (Type, Value, Environment) = {
    val env = new Environment()
    val ty = program.typecheck(env)
    println(ty)
    val value = program.evaluate(new Store())
    println(value)
    (ty, value, env )
  }
}
