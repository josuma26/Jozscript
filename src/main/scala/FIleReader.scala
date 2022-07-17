import language.expressions.Expression
import preprocessing.Parser

import java.io.{FileInputStream, FileReader, InputStreamReader}
import java.util.Scanner
import scala.collection.mutable

object FIleReader {
  def read(filename: String): Expression = {
    val program = readFileLines(filename)
    Parser.parse(program)
  }

  def readFileLines(filename: String): String = {
    val program = new mutable.StringBuilder()
    val scanner = new Scanner(new InputStreamReader(new FileInputStream(filename)))
    while (scanner.hasNextLine) {
      program.append(scanner.nextLine())
    }
    program.toString()
  }
}
