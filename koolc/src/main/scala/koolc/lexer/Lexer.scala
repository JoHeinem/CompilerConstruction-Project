package koolc
package lexer

import utils._
import scala.io.{BufferedSource, Source}
import java.io.File

object Lexer extends Pipeline[File, Iterator[Token]] {

  import Tokens._

  def run(ctx: Context)(f: File): Iterator[Token] = {
    val source: BufferedSource = Source.fromFile(f)
    import ctx.reporter._

    // Complete this file
    var colStart: Integer = 0
    var col: Integer = 0
    var row: Integer = 0
    var lastRun: Boolean = false
    var hastNextToken: Boolean = true
    // skip the first useless sign
    if (source.hasNext) {
      source.next()
      row = 1
      col = 1
    }

    new Iterator[Token] {
      def hasNext = {
        hastNextToken
      }

      def next: Token = {
        colStart = col
        if (!source.hasNext && row == 0 && col == 0) {
          hastNextToken = false
          return setPositionAndReturnToken(new Token(EOF))
        }
        if (lastRun && !source.hasNext) {
          hastNextToken = false
//          colStart -= 1
          return setPositionAndReturnToken(new Token(EOF))
        } else if (!source.hasNext) {
          lastRun = true
        }
        var token = new Token(BAD)
        source.ch match {
          case ':' => token = new Token(COLON); nextToken()
          case ';' => token = new Token(SEMICOLON); nextToken()
          case '.' => token = new Token(DOT); nextToken()
          case ',' => token = new Token(COMMA); nextToken()
          case '=' => token = checkForEqSignAndEquals()
          case '!' => token = new Token(BANG); nextToken()
          case '(' => token = new Token(LPAREN); nextToken()
          case ')' => token = new Token(RPAREN); nextToken()
          case '[' => token = new Token(LBRACKET); nextToken()
          case ']' => token = new Token(RBRACKET); nextToken()
          case '{' => token = new Token(LBRACE); nextToken()
          case '}' => token = new Token(RBRACE); nextToken()
          case '&' => token = checkForAnd()
          case '|' => token = checkForOr()
          case '<' => token = new Token(LESSTHAN); nextToken()
          case '+' => token = new Token(PLUS); nextToken()
          case '-' => token = new Token(MINUS); nextToken()
          case '*' => token = new Token(TIMES); nextToken()
          case '/' => token = checkForCommentAndDivision()
          case '\n' => token = handleNewLine()
          case '\t' => col += 3; nextToken(); token = next()
          case '\r' | ' ' => nextToken(); token = next()
          case '\"' => token = checkForStringLiterals()
          case c: Char => token = checkForExpression(c, source)
        }
        setPositionAndReturnToken(token)
      }

      private def handleNewLine(): Token = {
        if (nextToken()) {
          nextRow()
          next()
        } else {
          hastNextToken = false
          new Token(EOF)
        }
      }

      private def nextRow() = {
        row += 1
        col = 1
      }

      private def setPositionAndReturnToken(token: Token): Token = {
        val pos = Position.encode(row, colStart)
        token.setPos(f, pos)
      }

      // is needed because else the program crashes
      // e.g. when there is a identifier at the end
      private def nextToken(): Boolean = {
        //println("(" + row + ":" + col + ") ¤ " + source.ch)
        val canAdvance = source.hasNext
        if (canAdvance) {
          source.next()
          col += 1
        }
        canAdvance
      }

      private def checkForStringLiterals(): Token = {
        nextToken()
        val expression = new StringBuffer()
        while (!isLinebreak(source.ch) && source.hasNext && source.ch != '\"') {
          expression.append(source.ch)
          nextToken()
        }
        var token: Token = new Token(BAD)
        if (source.ch == '\"') {
          val exp: String = expression.toString
          token = new STRLIT(exp)
          nextToken()
        } else {
          error("invalid character: " + source.ch,
            setPositionAndReturnToken(token))
        }
        token
      }

      private def isLinebreak(c: Char): Boolean = {
        c == '\n' || c == '\r'
      }

      private def checkForCommentAndDivision(): Token = {
        val couldGoToNextToken: Boolean = nextToken()
        if (source.ch == '/' && couldGoToNextToken) {
          handleComment()
        } else if (source.ch == '*') {
          handleBlockComment()
        } else {
          new Token(DIV)
        }
      }

      private def handleComment(): Token = {
        while (source.ch != '\n' && source.hasNext) {
          nextToken()
        }
        next()
      }

      private def handleBlockComment(): Token = {
        var continue = true
        while (continue) {
          nextToken()
          if (!source.hasNext) {
            var token = new Token(BAD)
            error("unterminated block comment",
              setPositionAndReturnToken(token))
            return token
          }
          if (isLinebreak(source.ch)) {
            nextToken()
            nextRow()
          }
          if (source.ch == '*') {
            nextToken()
            if (source.ch == '/') {
              nextToken()
              continue = false
            }
          }
        }
        if (!source.hasNext)
          lastRun = true
        next()
      }

      private def checkForOr(): Token = {
        val couldGoToNextToken: Boolean = nextToken()
        var token: Token = new Token(BAD)
        if (source.ch == '|' && couldGoToNextToken) {
          token = new Token(OR)
          nextToken()
        } else {
          error("invalid character: " + source.ch,
            setPositionAndReturnToken(token))
        }
        token
      }

      private def checkForAnd(): Token = {
        val couldGoToNextToken: Boolean = nextToken()
        var token: Token = new Token(BAD)
        if (source.ch == '&' && couldGoToNextToken) {
          token = new Token(AND)
          nextToken()
        } else {
          error("invalid character: " + source.ch,
            setPositionAndReturnToken(token))
        }
        token
      }

      private def checkForEqSignAndEquals(): Token = {
        val couldGoToNextToken: Boolean = nextToken()
        if (source.ch == '=' && couldGoToNextToken) {
          nextToken()
          new Token(EQUALS)
        } else {
          new Token(EQSIGN)
        }
      }

      private def checkForExpression(c: Char, source: BufferedSource): Token = {
        if (c.isLetter) {
          checkForIdentifierAndKeywords(source)
        } else if (c.isDigit) {
          checkForIntegerLiterals(source)
        } else {
          val token = new Token(BAD)
          error("invalid character: " + source.ch,
            setPositionAndReturnToken(token))
          nextToken()
          token
        }
      }

      private def checkForIdentifierAndKeywords(source: BufferedSource): Token = {
        val expression = new StringBuffer()
        var c: Char = source.ch
        while ((c.isLetter || c.isDigit || c == '_') && source.hasNext) {
          expression.append(source.ch)
          nextToken()
          c = source.ch
        }
        if ((c.isLetter || c.isDigit || c == '_') && !source.hasNext) {
          expression.append(source.ch)
          lastRun = true
        }
        val exp: String = expression.toString
        stringMatchesKeyword(exp) match {
          case null => new ID(exp)
          case token: Token => token
        }
      }

      private def checkForIntegerLiterals(source: BufferedSource): Token = {
        var k = 0
        var c: Char = source.ch
        if (c.asDigit != 0) {
          while (c.isDigit && source.hasNext) {
            k = 10 * k + c.asDigit
            nextToken()
            c = source.ch
          }
          if (c.isDigit && !source.hasNext) {
            k = 10 * k + c.asDigit
            lastRun = true
          }
        } else {
          nextToken()
        }
        new INTLIT(k)
      }

      private def stringMatchesKeyword(string: String): Token = string match {
        case "object" => new Token(OBJECT)
        case "class" => new Token(CLASS)
        case "def" => new Token(DEF)
        case "var" => new Token(VAR)
        case "Unit" => new Token(UNIT)
        case "main" => new Token(MAIN)
        case "String" => new Token(STRING)
        case "extends" => new Token(EXTENDS)
        case "Int" => new Token(INT)
        case "Bool" => new Token(BOOLEAN)
        case "while" => new Token(WHILE)
        case "if" => new Token(IF)
        case "else" => new Token(ELSE)
        case "return" => new Token(RETURN)
        case "length" => new Token(LENGTH)
        case "true" => new Token(TRUE)
        case "false" => new Token(FALSE)
        case "this" => new Token(THIS)
        case "new" => new Token(NEW)
        case "println" => new Token(PRINTLN)
        case _ => null
      }
    }
  }
}
