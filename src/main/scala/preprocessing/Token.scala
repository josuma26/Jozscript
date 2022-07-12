package preprocessing

trait Token

/**
Symbols
*/
case class OParen() extends Token
case class CParen() extends Token
case class OBracket() extends Token
case class CBracket() extends Token
case class OCurly() extends Token
case class CCurly() extends Token
case class Arrow() extends Token
case class Number(value: Int) extends Token
case class BooleanToken(value: Boolean) extends Token
case class VariableToken(name: String) extends Token
case class Colon() extends Token
case class Comma() extends Token
case class Period() extends Token
case class SemiColon() extends Token
case class AssignToken() extends Token
case class FuncAssignToken() extends Token
case class Pipe() extends Token
case class NumBinOpOp(symbol: String) extends Token
case class BoolBinOpOP(symbol: String) extends Token
case class NumCompOpOP(symbol: String) extends Token

/**
 * Keywords
 */
case class Inductive() extends Token
case class TypeToken() extends Token
case class Lambda() extends Token
case class LetToken() extends Token
case class UnitToken() extends Token
case class AsToken() extends Token
case class MatchToken() extends Token
case class WithToken() extends Token

/**
 * Types
 */
case class BoolTypeToken() extends Token
case class NatTypeToken() extends Token
case class UnitTypeToken() extends Token

case class EOLToken() extends Token
