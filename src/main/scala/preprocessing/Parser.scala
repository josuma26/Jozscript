package preprocessing

import language.{App, Assign, Bool, BoolBinOp, BoolType, Expression, Func, Match, NatType, Num, NumBinOp, NumCompOP, Projection, Sequence, SumTy, Tuple, Type, UnitType, Var, Variant}

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map}

/**
 * Given a list of tokens, parse into an expression.
 *
 * When looking for an expression, any of these tokens will trigger an expression generator:
 * [if, let, skip, while, match, lambda, num, bool, ..
 */
object Parser {

  def parse(program: String): Expression = {
    val tokens = Lexer.lex(program)
    parse(tokens)
  }

  def parse(tokens: List[Token]): Expression = {
    val initialGen = ExpressionGenerator(expr => Expr(expr))
    tokens.foldLeft[Generator](initialGen)(parseOne) match {
      case Expr(expr) => expr
      case _ => throw new IllegalArgumentException("Unexpected EOL.")
    }
  }

  def parseOne(gen: Generator, token: Token): Generator = {
    gen match {
      case ExpressionGenerator(func) => parseExpression(func, token)
      case FromToken(func) => func(token)
      case Expr(expr) => parseAfterExpression(e=> Expr(e), expr, token)
      case TypeGenerator(func) => parseType(func, token)
    }
  }

  def parseExpression(func: Expression => Generator, token: Token): Generator = {
    token match {
      case Number(value) => parseAfterExpression(func, Num(value))
      case BooleanToken(value) => parseAfterExpression(func,Bool(value))
      case VariableToken(name) => parseAfterExpression(func,Var(name))
      case Lambda() => lambdaGen(func)
      case LetToken() => letGen(func)
      case OParen() => ExpressionGenerator(expr => FromToken(nextToken => {
        matchTokenThen[CParen](nextToken)(_ => func(expr))
      }))
      case MatchToken() => matchGenerator(func)
      case OBracket() => tupleGenerator(func, ListBuffer())
    }
  }

  def parseAfterExpression(func: Expression => Generator, expr: Expression, token: Token): Generator = {
    token match {
      case OParen() => ExpressionGenerator(nextExpr => FromToken(nextToken => {
        matchTokenThen[CParen](nextToken)(_ => func(App(expr, nextExpr)))
      }))
      case NumBinOpOp(symbol) => ExpressionGenerator(nextExpr => func(NumBinOp(symbol, expr, nextExpr)))
      case BoolBinOpOP(symbol) => ExpressionGenerator(nextExpr => func(BoolBinOp(symbol, expr, nextExpr)))
      case NumCompOpOP(symbol) => ExpressionGenerator(nextExpr => func(NumCompOP(symbol, expr, nextExpr)))
      case SemiColon() => ExpressionGenerator(nextExpr => func(Sequence(expr, nextExpr)))
      case Period() => FromToken(token => matchTokenThen[Number](token)(num => func(Projection(expr, num.value))))
      case Colon() => expr match {
        case Var(label) => variantGenerator(func, label)
        case _ => throw new IllegalArgumentException("Other uses of ':' not yet supported.")
      }
      case EOLToken() => func(expr)
      case _ => parseOne(func(expr), token)
    }
  }

  def parseAfterExpression(func: Expression => Generator, expr: Expression): Generator = {
    FromToken(token => parseAfterExpression(func, expr, token))
  }

  def parseType(func: Type => Generator, token: Token): Generator = {
    token match {
      case BoolTypeToken() => func(BoolType())
      case NatTypeToken() => func(NatType())
      case UnitTypeToken() => func(UnitType())
      case OCurly() => sumTypeGenerator(func, mutable.Map(), None)
    }
  }

  private def lambdaGen(afterGen: Expression => Generator) =
    FromToken(tokenVar => matchTokenThen[VariableToken](tokenVar)(variable => {
    FromToken(colon => matchTokenThen[Colon](colon)(_ => TypeGenerator(ty => {
      FromToken(comma => matchTokenThen[Comma](comma)(_ => ExpressionGenerator(
        body => afterGen(Func(variable.name, ty, body))
      )))
    })))
  }))

  private def letGen(afterGen: Expression => Generator) =
    FromToken(varToken => matchTokenThen[VariableToken](varToken)(variable => {
      FromToken(assignToken => matchTokenThen[AssignToken](assignToken)(_ => ExpressionGenerator(expr => {
        afterGen(Assign(variable.name, expr))
      })))
    }))

  private def tupleGenerator(afterGen: Expression => Generator, accum: ListBuffer[Expression]): Generator =
    FromToken {
      case Comma() => ExpressionGenerator(expr => tupleGenerator(afterGen, accum.addOne(expr)))
      case CBracket() => afterGen(Tuple(accum.toList))
      case token => parseExpression(expr => tupleGenerator(afterGen, accum.addOne(expr)), token)
    }

  private def variantGenerator(afterGen: Expression => Generator, label: String): Generator = {
    ExpressionGenerator(expr => FromToken(token => matchTokenThen[AsToken](token)(_ => {
      TypeGenerator(ty => afterGen(Variant(label, expr, ty)))
    })))
  }

  private def sumTypeGenerator(afterGen: Type => Generator, map: mutable.Map[String, Type], label: Option[String]): Generator = {
    FromToken {
      case VariableToken(label) => sumTypeGenerator(afterGen, map, Some(label))
      case Colon() if label.nonEmpty => TypeGenerator(ty => {
        sumTypeGenerator(afterGen, map += (label.get -> ty), None)
      })
      case Colon() => throw new IllegalArgumentException("Need label before ':'.")
      case Comma() => sumTypeGenerator(afterGen, map, None)
      case CCurly() => afterGen(SumTy(map.toMap))
    }
  }


  private def matchGenerator(afterGen: Expression => Generator): Generator = {
    ExpressionGenerator(expr => FromToken(tok => matchTokenThen[WithToken](tok)(_ => {
      FromToken(tok => matchTokenThen[OCurly](tok)(_ => {
        casesGenerator(afterGen, expr, mutable.Map(), None, None)
      }))
    })))
  }

  private def casesGenerator(afterGen: Expression => Generator, body: Expression,
                             cases: mutable.Map[String, (String, Expression)],
                             label: Option[String],
                             varName: Option[String]): Generator = {
    FromToken {
      case Pipe() => casesGenerator(afterGen, body, cases, None, None)
      case VariableToken(l) if label.isEmpty => casesGenerator(afterGen, body, cases, Some(l), None)
      case VariableToken(vName) if varName.isEmpty => casesGenerator(afterGen, body, cases, label, Some(vName))
      case Colon() if label.nonEmpty => casesGenerator(afterGen, body, cases, label, varName)
      case Colon() => throw new IllegalArgumentException("Need lable before ':'.")
      case CCurly() => afterGen(Match(body, cases.toMap))
      case FuncAssignToken() if label.nonEmpty && varName.nonEmpty => casesGenerator(afterGen, body, cases, label, varName)
      case token if label.nonEmpty && varName.nonEmpty => {
        parseExpression(expr => casesGenerator(afterGen, body, cases += (label.get -> (varName.get, expr)), None, None), token)
      }
      case token => throw new IllegalArgumentException(s"Syntax error, found $token")
    }
  }


  private def matchTokenThen[T <: Token](token: Token)(gen: T => Generator): Generator = {
    token match {
      case t: T => gen(t)
    }
  }

}
