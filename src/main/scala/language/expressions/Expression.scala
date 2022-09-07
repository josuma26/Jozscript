package language.expressions

import hoarelogic.logic.{And, Implies, Proposition, True}
import language._
import language.values.Value
import preprocessing.JFileReader

import java.io.{File, PrintWriter}

/**
 * Expressions in the language.
 * x | \x:T, e | e e | e +-*% e | e &&|| e
 *
 */
trait Expression {

  def evaluate(store: Store): Value

  def substitute(variable: String, value: Value): Expression

  def replace(variable: String, expr: Expression): Expression

  def typecheck(env: Environment): Type

  def proofObligation(pre: Proposition, post: Proposition, env: Environment): Proposition = Implies(pre, post)

  def pushThrough(pre: Proposition, env: Environment): Proposition = pre

  def impliesPushed(pre: Proposition, env: Environment): Proposition = True()

  def sameShapeAs(other: Expression): Boolean = {
    if (this.getClass.equals(other.getClass)) {
      checkSub(other.asInstanceOf[this.type])
    } else {
      false
    }
  }

  def typeSubs(typeVar: String, ty: Type): Expression

  protected def checkSub(other: this.type): Boolean

  def ensureHasType[T <: Expression](): T = {
    this match {
      case e: T => e
      case _ => throw new IllegalStateException(s"Expected $this to be of a different tyoe.")
    }
  }

  def printCoq(env: Environment): String = this.toString

  def generateCoqFile(fileName: String, env: Environment): Unit = {
    val fileBuilder = new PrintWriter(new File(fileName))
    writeCoqToFile(fileBuilder, env)
    fileBuilder.close()
  }

  protected def writeCoqToFile(writer: Appendable, env: Environment): Unit = {
    writeCoreLib(writer)
    writer.append(this.printCoq(env))
  }

  private def writeCoreLib(writer: Appendable): Unit = {
    val corelib = JFileReader.readFileLines("src/main/proofs/corelib.v")
    writer.append(corelib)
  }
}

case class Sequence(s1: Expression, s2: Expression) extends Expression {
  override def evaluate(store: Store): Value = {
    s1.evaluate(store); s2.evaluate(store)
  }

  override def substitute(variable: String, value: Value): Expression = {
    Sequence(s1.substitute(variable, value), s2.substitute(variable, value))
  }

  override def replace(variable: String, value: Expression): Expression = {
    Sequence(s1.replace(variable, value), s2.replace(variable, value))
  }

  override def proofObligation(pre: Proposition, post: Proposition, env: Environment): Proposition = {
    val pushed = s1.pushThrough(pre, env)
    val s1ImpliesPushed = s1.impliesPushed(pre, env)
    val s2Post = s2.proofObligation(pushed, post, env)
    And(s1ImpliesPushed, s2Post)
  }
  override def pushThrough(pre: Proposition, env: Environment): Proposition = {
    s2.pushThrough(s1.pushThrough(pre, env), env)
  }

  override def typecheck(env: Environment): Type = {
    s1.typecheck(env)
    s2.typecheck(env)
  }

  override def typeSubs(typeVar: String, ty: Type): Expression = {
    Sequence(s1.typeSubs(typeVar, ty), s2.typeSubs(typeVar, ty))
  }

  override def toString: String = s1.toString + ";\n" + s2.toString

  override def printCoq(env: Environment): String = s1.printCoq(env) + "\n\n" + s2.printCoq(env)

  override def checkSub(other: Sequence.this.type): Boolean = {
    this.s1.sameShapeAs(other.s1) && this.s2.sameShapeAs(other.s2)
  }
}