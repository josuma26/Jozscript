package preprocessing

import language.{Expression, Type}

/**
 * To parse an expression. Contains logic to create a different generator given a token
 */
trait Generator

case class Expr(expr: Expression) extends Generator
case class FromToken(func: Token => Generator) extends Generator
case class ExpressionGenerator(func: Expression => Generator) extends Generator
case class TypeGenerator(func: Type => Generator) extends Generator
