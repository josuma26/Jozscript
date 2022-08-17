package preprocessing

import hoarelogic.logic._
import language._
import language.expressions._
import language.values.{Bool, Num, TypeAbstraction, UnitVal}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
      case Expr(expr)  if token == SemiColon() => ExpressionGenerator(nextExpr => Expr(Sequence(expr, nextExpr)))
      case Expr(expr) => parseAfterExpression(e=> Expr(e), expr, token)
      case TypeGenerator(func) => parseType(func, token)
      case PropositionGenerator(func) => parseProposition(func, token)
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
      case OCurly() => curlySequenceGen(func)
      case MatchToken() => matchGenerator(func)
      case OBracket() => tupleGenerator(func, ListBuffer())
      case WhileToken() => whileGenerator(func)
      case IfToken() => ifGenerator(func)
      case TypeToken() => defineTypeGen(func)
      case DefToken() => defFunctionGen(func)
      case BigLambdaToken() => FromToken(matchTokenThen[VariableToken](_)(variable => {
        FromToken(matchTokenThen[Comma](_)(_ => ExpressionGenerator(expr => func(TypeAbstraction(variable.name, expr)))))
      }))
      case BeginPropToken() => FromToken(matchTokenThen[OBracket](_)(_ => PropositionGenerator(pre => {
        ExpressionGenerator(expr => PropositionGenerator(post => {
          parseAfterExpression(func, DecoratedProgram(pre, expr, post))
        }))
      })))
      case ImportToken() => FromToken(matchTokenThen[VariableToken](_)(varTok => {
        parseAfterExpression(func, ImportStatement(varTok.name))
      }))
      case _ => throw new IllegalArgumentException(s"Unexpected token: $token")
    }
  }

  def parseAfterExpression(func: Expression => Generator, expr: Expression, token: Token): Generator = {
    token match {
      case OParen() => ExpressionGenerator(nextExpr => FromToken(nextToken => {
        matchTokenThen[CParen](nextToken)(_ => parseAfterExpression(func, App(expr, nextExpr)))
      }))
      case OBracket() => TypeGenerator(ty => FromToken(matchTokenThen[CBracket](_)(_ => {
        parseAfterExpression(func, TypeApp(expr, ty))
      })))
      case NumBinOpOp(symbol) => ExpressionGenerator(nextExpr => func(NumBinOp(symbol, expr, nextExpr)))
      case BoolBinOpOP(symbol) => ExpressionGenerator(nextExpr => func(BoolBinOp(symbol, expr, nextExpr)))
      case NumCompOpOP(symbol) => ExpressionGenerator(nextExpr => func(NumCompOP(symbol, expr, nextExpr)))
      case Period() => FromToken(token => matchTokenThen[Number](token)(num => {
        parseAfterExpression(func,Projection(expr, num.value))
      }))
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
        parseAfterType(func, ty)
      })))
      case ForallToken() => FromToken(matchTokenThen[VariableToken](_)(name => {
        FromToken(matchTokenThen[Comma](_)(_ => TypeGenerator(ty => {
          parseAfterType(func, UniversalType(name.name, ty))
        })))
      }))
      case OBracket() => productTypeGenerator(func, ListBuffer())
    }
  }

  def parseAfterType(func: Type => Generator, ty: Type, token: Token): Generator = {
    token match {
      case Arrow() => TypeGenerator(nextTy => parseAfterType(func, FuncTy(ty, nextTy)))
      case NumCompOpOP(symbol) if symbol.equals("*") => productTypeGenerator(func, ListBuffer(ty))
      case OBracket() => TypeGenerator(nextTy => FromToken(matchTokenThen[CBracket](_)(_ => {
        parseAfterType(func, UnivTypeInstant(ty, nextTy))
      })))
      case _ => parseOne(func(ty), token)
    }
  }

  // TODO: maybe explicitly parse out ExprProp sub-props?
  def parseProposition(afterGen: Proposition => Generator, token: Token): Generator = {
    token match {
      case BeginPropToken() => FromToken(matchTokenThen[OBracket](_)(_ => PropositionGenerator(afterGen)))
      case TrueToken() => parseAfterProposition(afterGen, True())
      case FalseToken() => parseAfterProposition(afterGen, False())
      case NotToken() => PropositionGenerator(nextProp => parseAfterProposition(afterGen, Not(nextProp)))
      case other => parseExpression(expr => parseAfterProposition(afterGen, ExprProp(expr)), other)
    }
  }

  def parseAfterProposition(afterGen: Proposition => Generator, prop: Proposition): Generator = {
    FromToken(token => parseAfterProposition(afterGen, prop, token))
  }

  def parseAfterProposition(afterGen: Proposition => Generator, prop: Proposition, token: Token): Generator = {
    token match {
      case AndToken() => PropositionGenerator(nextProp => parseAfterProposition(afterGen, And(prop, nextProp)))
      case OrToken() => PropositionGenerator(nextProp => parseAfterProposition(afterGen, Or(prop, nextProp)))
      case Arrow() => PropositionGenerator(nextProp => parseAfterProposition(afterGen, Implies(prop, nextProp)))
      case CBracket() => afterGen(prop)
      case _ => parseOne(afterGen(prop), token)
    }
  }

  private def curlySequenceGen(afterGen: Expression => Generator): Generator = {
    ExpressionGenerator(expr => FromToken {
      case CCurly() => afterGen(expr)
      case SemiColon() => curlySequenceGen(nextExpr => afterGen(Sequence(expr, nextExpr)))
      case token => throw new IllegalArgumentException(s"Unepected $token")
    })
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
        parseAfterExpression(afterGen, Assign(variable.name, expr))
      })))
    }))

  private def defineTypeGen(afterGen: Expression => Generator) =
    FromToken(matchTokenThen[VariableToken](_)(varToken => FromToken(matchTokenThen[AssignToken](_)(_ => {
      TypeGenerator(ty => afterGen(TypeDefinition(varToken.name, ty)))
    }))))

  private def defFunctionGen(afterGen: Expression => Generator): Generator = {
    FromToken(matchTokenThen[VariableToken](_)(varName => FromToken(tok => {
      if (tok.equals(OBracket())) {
        typeListGen(afterGen, varName.name, new ListBuffer())
      } else {
        matchTokenThen[OParen](tok)( _ => {
          defFunctionGen(afterGen, varName.name, List(), ListBuffer(), None)
        })
      }
    })))
  }

  private def typeListGen(afterGen: Expression => Generator, varName: String, types: ListBuffer[String]): Generator = {
    FromToken {
      case Comma() => typeListGen(afterGen, varName, types)
      case CBracket() => FromToken(matchTokenThen[OParen](_)( _ => {
        defFunctionGen(afterGen, varName, types.toList.reverse, ListBuffer(), None)
      }))
      case VariableToken(name) => typeListGen(afterGen, varName, types += name)
      case token => throw new IllegalArgumentException(s"Unexpected token $token")
    }
  }

  private def defFunctionGen(afterGen: Expression => Generator, funcName: String, types: List[String],
                             args: ListBuffer[(String, Type)], argName: Option[String]): Generator = {
    FromToken {
      case Comma() => defFunctionGen(afterGen, funcName, types,  args, argName)
      case VariableToken(name) if argName.isEmpty => defFunctionGen(afterGen, funcName, types, args, Some(name))
      case VariableToken(name) =>
        parseAfterType(ty => defFunctionGen(afterGen, funcName, types, args.addOne((argName.get, ty)), None) , TypeAlias(name))
      case Colon() if argName.nonEmpty =>
        parseType(ty => defFunctionGen(afterGen, funcName, types, args += ((argName.get,  ty)), None))
      case CParen() => FromToken(matchTokenThen[Colon](_)(_ => TypeGenerator(retTy => {
        FromToken(matchTokenThen[AssignToken](_)(_ => ExpressionGenerator(expr => {
          afterGen(FunctionDefinition(funcName, types, args.toMap, retTy, expr))
        })))
      })))
      case token if argName.nonEmpty =>
        parseType(ty => defFunctionGen(afterGen, funcName, types, args += (argName.get -> ty), None), token)
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
  while (cond) [inv?] do {
    (body)
  }
   */
  private def whileGenerator(nextGen: Expression => Generator): Generator = {
    ExpressionGenerator(cond => FromToken(tok => {
      if (tok == BeginPropToken()) {
        FromToken(matchTokenThen[OBracket](_)(_ => {
          PropositionGenerator(inv => FromToken(matchTokenThen[DoToken](_)(_ => {
            ExpressionGenerator(body => nextGen(WhileLoop(cond, body, inv)))
          })))
        }))
      } else if (tok == DoToken()) {
        ExpressionGenerator(body => nextGen(WhileLoop(cond, body)))
      } else {
        throw new IllegalArgumentException(s"Unexpected $tok")
      }
    }))
  }

  private def ifGenerator(nextGen: Expression => Generator): Generator = {
    ExpressionGenerator(cond => FromToken(matchTokenThen[ThenToken](_)(_ => {
      ExpressionGenerator(t1 => FromToken(matchTokenThen[ElseToken](_)(_ => {
        ExpressionGenerator(t2 => nextGen(IfStatement(cond, t1, t2)))
      })))
    })))
  }


  private def matchTokenThen[T <: Token](token: Token)(gen: T => Generator): Generator = {
    token match {
      case t: T => gen(t)
    }
  }

}
