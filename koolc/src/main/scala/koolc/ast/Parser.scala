package koolc
package ast

import utils._
import Trees._
import lexer._
import lexer.Tokens._

import scala.collection.mutable.ListBuffer

object Parser extends Pipeline[Iterator[Token], Program] {
  def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._

    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind::more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    def currentPos: Positioned = { currentToken }

    // Grammar in BNF can be found here:
    // http://www.csc.kth.se/~phaller/compilers/labs/labs2.html

    def parseGoal: Program = {
      val pos = currentPos
      val mainObject = parseMainObject
      var classes = new ListBuffer[ClassDecl]()
      while (currentToken.kind != EOF)
        classes += parseClassDecl
      eat(EOF)
      (new Program(mainObject, classes.toList)).setPos(pos)
    }

    def parseMainObject: MainObject = {
      val pos = currentPos
      eat(OBJECT)
      val identifier = parseIdentifier
      eat(LBRACE)
      eat(DEF)
      eat(MAIN)
      eat(LPAREN)
      eat(RPAREN)
      eat(COLON)
      eat(UNIT)
      eat(EQSIGN)
      eat(LBRACE)
      var statements = new ListBuffer[StatTree]()
      while (currentToken.kind != RBRACE)
        statements += parseStatement
      eat(RBRACE)
      eat(RBRACE)
      (new MainObject(identifier, statements.toList)).setPos(pos)
    }

    def parseClassDecl: ClassDecl = {
      val pos = currentPos
      eat(CLASS)
      val identifier = parseGenericIdentifier
      var inheritance: Option[Identifier] = None
      if (currentToken.kind == EXTENDS) {
        eat(EXTENDS)
       inheritance = Some(parseIdentifier)
      }
      eat(LBRACE)
      var variables = new ListBuffer[VarDecl]()
      while(currentToken.kind != DEF && currentToken.kind != RBRACE)
        variables += parseVarDecl
      var methods = new ListBuffer[MethodDecl]()
      while(currentToken.kind != RBRACE)
        methods += parseMethodDecl
      eat(RBRACE)
      (new ClassDecl(identifier, inheritance, variables.toList, methods.toList)).setPos(pos)
    }

    def parseVarDecl: VarDecl = {
      val pos = currentPos
      eat(VAR)
      val identifier = parseIdentifier
      eat(COLON)
      val tpe = parseType
      tpe match {
        case Identifier(_, t) => identifier.genericType = t
        case _ =>
      }
      eat(SEMICOLON)
      (new VarDecl(tpe, identifier)).setPos(pos)
    }

    def parseMethodDecl: MethodDecl = {
      val pos = currentPos
      eat(DEF)
      val identifier = parseIdentifier
      eat(LPAREN)
      var params = new ListBuffer[Formal]
      if (currentToken.kind != RPAREN) {
        params += parseFormal
      }
      while(currentToken.kind == COMMA) {
        eat(COMMA)
        params += parseFormal
      }
      eat(RPAREN)
      eat(COLON)
      var retType = parseType
      eat(EQSIGN)
      eat(LBRACE)
      var variables = new ListBuffer[VarDecl]
      while (currentToken.kind == VAR)
        variables += parseVarDecl
      var statements = new ListBuffer[StatTree]
      while (currentToken.kind != RETURN)
        statements += parseStatement
      eat(RETURN)
      val retExpr = parseExpression
      eat(SEMICOLON)
      eat(RBRACE)
      (new MethodDecl(retType, identifier, params.toList, variables.toList, statements.toList, retExpr)).setPos(pos)
    }

    def parseFormal: Formal = {
      val pos = currentPos
      val identifier = parseIdentifier
      eat(COLON)
      val tpe = parseType
      (new Formal(tpe, identifier)).setPos(pos)
    }

    def parseType: TypeTree = {
      val pos = currentPos
      if (currentToken.kind == INT) {
        eat(INT)
        if (currentToken.kind == LBRACKET) {
          eat(LBRACKET)
          eat(RBRACKET)
          (new IntArrayType).setPos(pos)
        } else {
          (new IntType).setPos(pos)
        }
      } else if (currentToken.kind == BOOLEAN) {
        eat(BOOLEAN)
        (new BooleanType).setPos(pos)
      } else if (currentToken.kind == STRING) {
        eat(STRING)
        (new StringType).setPos(pos)
      } else {
        parseGenericIdentifier
      }
    }

    def parseStatement: StatTree = {
      val pos = currentPos
      if (currentToken.kind == LBRACE) {
        eat(LBRACE)
        var statements = new ListBuffer[StatTree]()
        while (currentToken.kind != RBRACE)
          statements += parseStatement
        eat(RBRACE)
        (new Block(statements.toList)).setPos(pos)
      } else if (currentToken.kind == IF) {
        eat(IF)
        eat(LPAREN)
        val expression = parseExpression
        eat(RPAREN)
        val statement = parseStatement
        var opt: Option[StatTree] = None
        if (currentToken.kind == ELSE) {
          eat(ELSE)
          opt = Some(parseStatement)
        }
        (new If(expression, statement, opt)).setPos(pos)
      } else if (currentToken.kind == WHILE) {
        eat(WHILE)
        eat(LPAREN)
        val expression = parseExpression
        eat(RPAREN)
        val statement = parseStatement
        (new While(expression, statement)).setPos(pos)
      } else if (currentToken.kind == PRINTLN) {
        eat(PRINTLN)
        eat (LPAREN)
        val expression = parseExpression
        eat (RPAREN)
        eat(SEMICOLON)
        (new Println(expression)).setPos(pos)
      } else {
        val identifier = parseIdentifier
        if (currentToken.kind == LBRACKET) {
          eat(LBRACKET)
          val exp1 = parseExpression
          eat(RBRACKET)
          eat(EQSIGN)
          val exp2 = parseExpression
          eat(SEMICOLON)
          (new ArrayAssign(identifier, exp1, exp2)).setPos(pos)
        } else {
          eat(EQSIGN)
          val expression = parseExpression
          eat(SEMICOLON)
          (new Assign(identifier, expression)).setPos(pos)
        }
      }
    }

    def parseExpression: ExprTree = {
      var or = parseAnd
      while(currentToken.kind == OR) {
        eat(OR)
        or = (new Or(or, parseAnd)).setPos(or)
      }
      or
    }

    def parseAnd: ExprTree = {
      var and = parseEqLess
      while (currentToken.kind == AND) {
        eat(AND)
        and = (new And(and, parseEqLess)).setPos(and)
      }
      and
    }

    def parseEqLess: ExprTree = {
      var eqless = parsePlusMinus
      while(currentToken.kind == EQUALS ||
      currentToken.kind == LESSTHAN) {
        if (currentToken.kind == EQUALS) {
          eat(EQUALS)
          eqless = (new Equals(eqless, parsePlusMinus)).setPos(eqless)
        } else if (currentToken.kind == LESSTHAN) {
          eat(LESSTHAN)
          eqless = (new LessThan(eqless, parsePlusMinus)).setPos(eqless)
        }
      }
      eqless
    }

    def parsePlusMinus: ExprTree = {
      var plusminus = parseMulDiv
      while(currentToken.kind == PLUS ||
      currentToken.kind == MINUS) {
        if (currentToken.kind == PLUS) {
          eat(PLUS)
          plusminus = (new Plus(plusminus, parseMulDiv)).setPos(plusminus)
        } else if (currentToken.kind == MINUS) {
          eat(MINUS)
          plusminus = (new Minus(plusminus, parseMulDiv)).setPos(plusminus)
        }
      }
      plusminus
    }

    def parseMulDiv: ExprTree = {
      var muldiv = parseDot
      while(currentToken.kind == TIMES ||
      currentToken.kind == DIV) {
        if (currentToken.kind == TIMES) {
          eat(TIMES)
          muldiv = (new Times(muldiv, parseDot)).setPos(muldiv)
        } else if (currentToken.kind == DIV) {
          eat(DIV)
          muldiv = (new Div(muldiv, parseDot)).setPos(muldiv)
        }
      }
      muldiv
    }

    def parseDot: ExprTree = {
      var rest = parseRest
      if (currentToken.kind == DOT) {
        while (currentToken.kind == DOT) {
          eat(DOT)
          if (currentToken.kind == LENGTH) {
            eat(LENGTH)
            rest = (new ArrayLength(rest)).setPos(rest)
          } else {
            val identifier = parseIdentifier
            eat(LPAREN)
            var args = new ListBuffer[ExprTree]()
            if (currentToken.kind != RPAREN)
              args += parseExpression
            while (currentToken.kind == COMMA) {
              eat(COMMA)
              args += parseExpression
            }
            eat(RPAREN)
            rest = (new MethodCall(rest, identifier, args.toList)).setPos(rest)
          }
        }
      } else if (currentToken.kind == LBRACKET) {
          eat(LBRACKET)
          val expr = parseExpression
          eat(RBRACKET)
          rest = (new ArrayRead(rest, expr)).setPos(rest)

      }
      rest
    }

    def parseRest: ExprTree = {
      val pos = currentPos
      if (currentToken.kind == INTLITKIND) {
        var value = currentToken.asInstanceOf[INTLIT].value
        eat(INTLITKIND)
        (new IntLit(value)).setPos(pos)
      } else if (currentToken.kind == STRLITKIND) {
        var value = currentToken.asInstanceOf[STRLIT].value
        eat(STRLITKIND)
        (new StringLit(value)).setPos(pos)
      } else if (currentToken.kind == TRUE) {
        eat(TRUE)
        (new True).setPos(pos)
      } else if (currentToken.kind == FALSE) {
        eat(FALSE)
        (new False).setPos(pos)
      } else if (currentToken.kind == THIS) {
        eat(THIS)
        (new This).setPos(pos)
      } else if (currentToken.kind == NEW) {
        eat(NEW)
        if (currentToken.kind == INT) {
          eat(INT)
          eat(LBRACKET)
          val expression = parseExpression
          eat(RBRACKET)
          (new NewIntArray(expression)).setPos(pos)
        } else {
          val identifier = parseGenericIdentifier
          eat(LPAREN)
          eat(RPAREN)
          (new New(identifier)).setPos(pos)
        }
      } else if (currentToken.kind == LPAREN) {
        eat(LPAREN)
        val e = parseExpression
        eat(RPAREN)
        e
      } else if (currentToken.kind == BANG) {
        eat(BANG)
        (new Not(parseDot)).setPos(pos)
      } else {
        parseIdentifier
      }
    }

    def parseIdentifier: Identifier = {
      val pos = currentPos
      var value: String = ""
      if (currentToken.kind == IDKIND)
        value = currentToken.asInstanceOf[ID].value
      eat(IDKIND)
      (new Identifier(value, None)).setPos(pos)
    }

    def parseGenericIdentifier: Identifier = {
      val pos = currentPos
      val identifier: Identifier = parseIdentifier
      var genericType: Option[TypeTree] = None
      if (currentToken.kind == LBRACKET) {
        eat(LBRACKET)
        val pos2 = currentPos
        genericType = currentToken.kind match {
          case STRING =>
            eat(STRING)
            Some((new StringType).setPos(pos2))
          case IDKIND =>
            Some(parseGenericIdentifier)
          case _ =>
            error("Primitive types not allowed for generics", pos2)
            readToken
            None
        }
        eat(RBRACKET)
      }
      identifier.genericType = genericType
      identifier.setPos(pos)
    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
