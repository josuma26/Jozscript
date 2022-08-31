
import language.expressions.Expression
import language.values.Value
import language.{Environment, Store, Type}
import preprocessing.JFileReader

object main {

  private val PROGRAM_PATH = "src/main/scala/programs/"
  private val OUTPUT_PATH = "src/main/proofs/"

  def main(args: Array[String]): Unit = {
    verifyProgram("fact")
  }

  private def verifyProgram(progName: String) = {
    val programFile = PROGRAM_PATH + progName + ".txt"
    val outputFile = OUTPUT_PATH + progName + "_proof.v"
    JFileReader.read(programFile).generateCoqFile(outputFile)
  }

  private def executeFromFile(fileName: String): (Type, Value) = {
    val expr = JFileReader.read(fileName)
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
