package preprocessing

import language.Type
import language.expressions.Expression

/**
 * To parse an expression. Contains logic to create a different generator given a token
 */
sealed trait Generator

case class Expr(expr: Expression) extends Generator
case class FromToken(func: Token => Generator) extends Generator
case class ExpressionGenerator(func: Expression => Generator) extends Generator
case class TypeGenerator(func: Type => Generator) extends Generator
