package preprocessing

import language.expressions.{App, BoolBinOp, Expression, Match, NumBinOp, NumCompOP, PatternMatch, Projection, Sequence, TupleExpression, Var, VariantExpression}
import language.values.{Bool, Func, Num, UnitVal}
import language.{Assign, BoolType, Environment, ExpressionPattern, FuncTy, FunctionDefinition, LabelBinderPattern, NatType, Pattern, ProductTy, Store, SumTy, Type, TypeAlias, TypeDefinition, UnitType, WhileLoop, values}

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
      case UnitToken() => parseAfterExpression(func, UnitVal())
      case Lambda() => lambdaGen(func)
      case LetToken() => letGen(func)
      case OParen() => ExpressionGenerator(expr => FromToken(nextToken => {
        matchTokenThen[CParen](nextToken)(_ => parseAfterExpression(func, expr))
      }))
      case MatchToken() => matchGenerator(func)
      case OBracket() => tupleGenerator(func, ListBuffer())
      case WhileToken() => whileGenerator(func)
      case TypeToken() => defineTypeGen(func)
      case DefToken() => defFunctionGen(func)
      case _ => throw new IllegalArgumentException(s"Unexpected token: $token")
    }
  }

  def parseAfterExpression(func: Expression => Generator, expr: Expression, token: Token): Generator = {
    token match {
      case OParen() => ExpressionGenerator(nextExpr => FromToken(nextToken => {
        matchTokenThen[CParen](nextToken)(_ => parseAfterExpression(func, App(expr, nextExpr)))
      }))
      case NumBinOpOp(symbol) => ExpressionGenerator(nextExpr => func(NumBinOp(symbol, expr, nextExpr)))
      case BoolBinOpOP(symbol) => ExpressionGenerator(nextExpr => func(BoolBinOp(symbol, expr, nextExpr)))
      case NumCompOpOP(symbol) => ExpressionGenerator(nextExpr => func(NumCompOP(symbol, expr, nextExpr)))
      case SemiColon() => ExpressionGenerator(nextExpr => func(Sequence(expr, nextExpr)))
      case Period() => FromToken(token => matchTokenThen[Number](token)(num => {
        parseAfterExpression(func,Projection(expr, num.value))
      }))
      case Colon() => expr match {
        case Var(label) => variantGenerator(func, label)
        case _ => throw new IllegalArgumentException("Other uses of ':' not yet supported.")
      }
      case EOLToken() => func(expr)
      case SemiColon() => ExpressionGenerator(nextExpr => func(Sequence(expr, nextExpr)))
      case _ => parseOne(func(expr), token)
    }
  }

  def parseAfterExpression(func: Expression => Generator, expr: Expression): Generator = {
    FromToken(token => parseAfterExpression(func, expr, token))
  }

  def parseType(func: Type => Generator): Generator = {
    FromToken(parseType(func, _))
  }
  def parseType(func: Type => Generator, token: Token): Generator = {
    token match {
      case BoolTypeToken() => parseAfterType(func, BoolType())
      case NatTypeToken() => parseAfterType(func, NatType())
      case UnitTypeToken() => parseAfterType(func, UnitType())
      case VariableToken(varName) => parseAfterType(func, TypeAlias(varName))
      case OCurly() => sumTypeGenerator(func, mutable.Map(), None)
      case OParen() => TypeGenerator(ty => FromToken(matchTokenThen[CParen](_)(_ => {
        func(ty)
      })))
      case OBracket() => productTypeGenerator(func, ListBuffer())
    }
  }

  def parseAfterType(func: Type => Generator, ty: Type, token: Token): Generator = {
    token match {
      case Arrow() => TypeGenerator(nextTy => func(FuncTy(ty, nextTy)))
      case NumCompOpOP(symbol) if symbol.equals("*") => productTypeGenerator(func, ListBuffer(ty))
      case _ => parseOne(func(ty), token)
    }
  }

  private def productTypeGenerator(afterGen: Type => Generator, types: ListBuffer[Type]): Generator =
    FromToken {
      case NumBinOpOp(symbol) if symbol.equals("*") => productTypeGenerator(afterGen, types)
      case CBracket() => afterGen(ProductTy(types.toList))
      case token => parseType(ty => productTypeGenerator(afterGen, types.addOne(ty)), token)
    }

  def parseAfterType(func: Type => Generator, ty: Type): Generator = FromToken(parseAfterType(func, ty, _))

  private def lambdaGen(afterGen: Expression => Generator) =
    FromToken(tokenVar => matchTokenThen[VariableToken](tokenVar)(variable => {
    FromToken(colon => matchTokenThen[Colon](colon)(_ => TypeGenerator(ty => {
      FromToken(comma => matchTokenThen[Comma](comma)(_ => ExpressionGenerator(
        body => afterGen(values.Func(variable.name, ty, body))
      )))
    })))
  }))

  private def letGen(afterGen: Expression => Generator) =
    FromToken(varToken => matchTokenThen[VariableToken](varToken)(variable => {
      FromToken(assignToken => matchTokenThen[AssignToken](assignToken)(_ => ExpressionGenerator(expr => {
        afterGen(Assign(variable.name, expr))
      })))
    }))

  private def defineTypeGen(afterGen: Expression => Generator) =
    FromToken(matchTokenThen[VariableToken](_)(varToken => FromToken(matchTokenThen[AssignToken](_)(_ => {
      TypeGenerator(ty => afterGen(TypeDefinition(varToken.name, ty)))
    }))))

  private def defFunctionGen(afterGen: Expression => Generator): Generator = {
    FromToken(matchTokenThen[VariableToken](_)(varName => FromToken(matchTokenThen[OParen](_)( _ => {
      defFunctionGen(afterGen, varName.name, mutable.Map(), None)
    }))))
  }

  private def defFunctionGen(afterGen: Expression => Generator, funcName: String,
                             args: mutable.Map[String, Type], argName: Option[String]): Generator = {
    FromToken {
      case Comma() => defFunctionGen(afterGen, funcName, args, argName)
      case VariableToken(name) if argName.isEmpty => defFunctionGen(afterGen, funcName, args, Some(name))
      case VariableToken(name) =>
        parseAfterType(ty => defFunctionGen(afterGen, funcName, args += (argName.get -> ty), None) , TypeAlias(name))
      case Colon() if argName.nonEmpty =>
        parseType(ty => defFunctionGen(afterGen, funcName, args += (argName.get -> ty), None))
      case CParen() => FromToken(matchTokenThen[Colon](_)(_ => TypeGenerator(retTy => {
        FromToken(matchTokenThen[AssignToken](_)( _ => FromToken(matchTokenThen[OCurly](_)(_ => {
          ExpressionGenerator(expr => FromToken(matchTokenThen[CCurly](_)(_ => {
            afterGen(FunctionDefinition(funcName, args.toMap, retTy, expr))
          })))
        }))))
      })))
      case token if argName.nonEmpty =>
        parseType(ty => defFunctionGen(afterGen, funcName, args += (argName.get -> ty), None), token)
      case token => throw new IllegalArgumentException(s"Unexpected token. Found: $token")

    }
  }

  private def tupleGenerator(afterGen: Expression => Generator, accum: ListBuffer[Expression]): Generator =
    FromToken {
      case Comma() => ExpressionGenerator(expr => tupleGenerator(afterGen, accum.addOne(expr)))
      case CBracket() => afterGen(TupleExpression(accum.toList))
      case token => parseExpression(expr => tupleGenerator(afterGen, accum.addOne(expr)), token)
    }

  private def variantGenerator(afterGen: Expression => Generator, label: String): Generator = {
    ExpressionGenerator(expr => FromToken(token => matchTokenThen[AsToken](token)(_ => {
      TypeGenerator(ty => afterGen(VariantExpression(label, expr, ty)))
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
        casesGenerator(afterGen, expr, ListBuffer(), None)
      }))
    })))
  }

  private def casesGenerator(afterGen: Expression => Generator, body: Expression,
                             cases: ListBuffer[(Pattern, Expression)],
                             pattern: Option[Pattern]): Generator = {
    FromToken {
      case Pipe() => casesGenerator(afterGen, body, cases, pattern)
      case FuncAssignToken() if pattern.nonEmpty => casesGenerator(afterGen, body, cases, pattern)
      case CCurly() => afterGen(PatternMatch(body, cases.toList))
      case token if pattern.nonEmpty =>
        parseExpression(expr => casesGenerator(afterGen, body, cases.addOne((pattern.get, expr)), None), token)
      case token => parsePattern(patt => casesGenerator(afterGen, body, cases, Some(patt)), token)
    }
  }

  private def parsePattern(func: Pattern => Generator, token: Token): Generator = {
    token match {
      case VariableToken(varName) => FromToken(tok => {
        if (tok.equals(Colon())) {
          FromToken(parsePattern(patt => func(LabelBinderPattern(varName, patt)),_))
        } else {
          parseAfterExpression(expr => func(ExpressionPattern(expr)), Var(varName), tok)
        }
      })
      case tok => parseExpression(expr => func(ExpressionPattern(expr)), tok)
    }
  }

  /*
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

   */

  private def whileGenerator(nextGen: Expression => Generator): Generator = {
    ExpressionGenerator(cond => FromToken(matchTokenThen[DoToken](_)(_ => {
      ExpressionGenerator(body => nextGen(WhileLoop(cond, body)))
    })))
  }


  private def matchTokenThen[T <: Token](token: Token)(gen: T => Generator): Generator = {
    token match {
      case t: T => gen(t)
    }
  }

}
