package preprocessing

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait TokenGenerator {
  def matches(word: String): Boolean
  def generate(word: String): Token
}
case class OneOf(options: List[String], gen: String => Token) extends TokenGenerator {
  override def matches(word: String): Boolean = options.contains(word)

  override def generate(word: String): Token = gen(word)
}

case class ExactMatch(word: String, token: Token) extends TokenGenerator {
  override def matches(word: String): Boolean = word.equals(this.word)

  override def generate(word: String): Token = token
}

case class Predicate(pred: String => Boolean, gen: String => Token) extends TokenGenerator {
  override def matches(word: String): Boolean = pred(word)

  override def generate(word: String): Token = gen(word)
}
/**
 * Given a String, create a list of Tokens.
 *
 * First, create a list of words by splitting all clumps of alphanumeric digits and symbols into individual words.
 */
object Lexer {

  private val generators = List(
    OneOf(List("true", "false"), w => BooleanToken(w.toBoolean)),
    ExactMatch("unit", UnitToken()),
    ExactMatch("lambda", Lambda()),
    ExactMatch("let", LetToken()),
    ExactMatch("while", WhileToken()),
    ExactMatch("do", DoToken()),
    ExactMatch(":=", AssignToken()),
    ExactMatch(":", Colon()),
    ExactMatch(",", Comma()),
    ExactMatch("(", OParen()),
    ExactMatch(")", CParen()),
    ExactMatch("[", OBracket()),
    ExactMatch("]", CBracket()),
    ExactMatch("{", OCurly()),
    ExactMatch("}", CCurly()),
    ExactMatch(";", SemiColon()),
    ExactMatch(".", Period()),
    OneOf(List("+","-","*","/","%"), NumBinOpOp),
    OneOf(List("&&", "||"), BoolBinOpOP),
    OneOf(List("<",">","=",">=","<=","=","!="), NumCompOpOP),
    Predicate(word => word.forall(c => c.isDigit), w => Number(w.toInt)),
    ExactMatch("->", Arrow()),
    ExactMatch("=>", FuncAssignToken()),
    ExactMatch("Bool", BoolTypeToken()),
    ExactMatch("Nat", NatTypeToken()),
    ExactMatch("Unit", UnitTypeToken()),
    ExactMatch("as", AsToken()),
    ExactMatch("match", MatchToken()),
    ExactMatch("with", WithToken()),
    ExactMatch("type", TypeToken()),
    ExactMatch("def", DefToken()),
    ExactMatch("Lambda", BigLambdaToken()),
    ExactMatch("forall", ForallToken()),
    ExactMatch("|", Pipe()),
    ExactMatch("if", IfToken()),
    ExactMatch("then", ThenToken()),
    ExactMatch("else", ElseToken())
  )

  def lex(program: String): List[Token] = {
    val tokens = ListBuffer[Token]()
    val words = getWords(program)
    words.foreach(word => addToken(tokens, word))
    tokens.addOne(EOLToken())
    tokens.toList
  }

  private def addToken(tokens: ListBuffer[Token], word: String): Unit = {
    generators.foreach(gen =>
      if (gen.matches(word)) {
        tokens.addOne(gen.generate(word))
        return
      }
    )
    tokens.addOne(VariableToken(word))
  }

  private def getWords(program: String): List[String] = {
    val specialCharacters = List(';','(',')','[',']',';','{','}')
    val words = ListBuffer[String]()
    val wordBuilder = new mutable.StringBuilder()
    var lastCharacterSpecial = false

    program.foreach(char => {
      if (char.isWhitespace) {
        addAndClear(words, wordBuilder)
        lastCharacterSpecial = false
      } else {
        /*
        If lastCharacteSpecial, then alphanumeric digits should start a new word, special should be addeed.
        If !lastCharacterSpecial, alphanumeric digits should be added, special should start a new word
        */
        if (specialCharacters.contains(char)) {
          addAndClear(words, wordBuilder)
          wordBuilder.append(char)
          addAndClear(words, wordBuilder)
          lastCharacterSpecial = false
        } else {
          if ((lastCharacterSpecial && char.isLetterOrDigit) || (!lastCharacterSpecial && !char.isLetterOrDigit)) {
            addAndClear(words, wordBuilder)
          }
          lastCharacterSpecial = !char.isLetterOrDigit
          wordBuilder.append(char)
        }
      }
    })
    addAndClear(words, wordBuilder)
    words.toList
  }

  private def addAndClear(words: ListBuffer[String], wordBuilder: mutable.StringBuilder): Unit = {
    if (wordBuilder.nonEmpty) {
      words.addOne(wordBuilder.toString())
      wordBuilder.clear()
    }
  }

}
