
import hoarelogic.logic.{ExprProp, Proposition, True}
import language.expressions.{Expression, Sequence}
import language.values.Value
import language.{Environment, Store, Type, WhileLoop}
import preprocessing.Parser

object main {
  def main(args: Array[String]): Unit = {
    //fromFile("src/main/scala/programs/lists.txt")
    ex()

  }

  private def ex() = {
    val setup = Parser.parse("let n := 5; let a := 1")
    val loopBody = Parser.parse("let a := n * a; let n := n - 1")
    val cond = Parser.parse("n > 0")
    val inv = ExprProp(Parser.parse("(a * fact(n)) = 120"))
    val loop = WhileLoop(cond, loopBody, inv )
    val program = Sequence(setup, loop)
    val post = ExprProp(Parser.parse("a = 120"))
    proofObligtion(True(), post, program)
  }

  private def fromFile(fileName: String): (Type, Value) = {
    val expr = FIleReader.read(fileName)
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
    val aeq2 = ExprProp(Parser.parse("a = fact(n)"))
    proofObligtion(True(), aeq2, program)
    (ty, value)
  }
}
