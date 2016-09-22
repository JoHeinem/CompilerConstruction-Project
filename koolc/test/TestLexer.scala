import java.io.{FileOutputStream, PrintStream, File, OutputStream, InputStream}

import koolc.ast.Printer
import koolc.lexer.Tokens._
import koolc.lexer.{PrintTokens, Token, Lexer}
import koolc.utils.{Reporter, Context, Pipeline}
import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

import scala.util.Random

/**
 *
 * Created by johannes on 3/11/15.
 */
class TestLexer extends FlatSpec with Matchers with BeforeAndAfter {

  val fileDir: String = "../test/test.txt"
  val commandForReferenceCompiler: String =
    "scala -cp ../koolc_2.11-1.2.jar:../cafebabe_2.11-1.2.jar koolc.Main --tokens "
  var fos: FileOutputStream = null
  var file: File = null
  var pipeline: Pipeline[File, Iterator[Token]] = null
  var ctx: Context = null

  before {
    pipeline = Lexer andThen PrintTokens
    val args: Array[String] = Array(fileDir)
    ctx = processOptions(args)


    file = new File(fileDir)
    fos = new FileOutputStream(file)

    // if file doesnt exists, then create it
    if (!file.exists()) {
      file.createNewFile()
    }
  }

  after {
    fos.close()
  }

  /**
   * Copied from the main method. I was just to lazy to set it up properly on my own
   * @param args:
   * @return
   */
  def processOptions(args: Array[String]): Context = {

    val reporter = new Reporter()
    var outDir: Option[File] = None
    var files: List[File] = Nil

    def processOption(args: List[String]): Unit = args match {
      case "-d" :: out :: args =>
        outDir = Some(new File(out))
        processOption(args)

      case f :: args =>
        files = new File(f) :: files
        processOption(args)

      case Nil =>
    }

    processOption(args.toList)

    if (files.size != 1) {
      reporter.fatal("Exactly one file expected, " + files.size + " file(s) given.")
    }

    Context(reporter = reporter, file = files.head, outDir = outDir, printTokens = true)
  }


  /**
   * Writes the provided String into to the file for testing
   * @param string: to write in the file
   * @return
   */
  private def setUpFile(string: String): Any = {
    // get the content in bytes
    val contentInBytes = string.getBytes
    fos.write(contentInBytes)
    fos.flush()
    fos.close()
  }

  /**
   * Internal class needed to handle the redirecting of the outputstream
   * @param out
   */
  class Interceptor(out: java.io.OutputStream) extends PrintStream(out) {
    var str: String = ""

    @Override
    def Interceptor(out: OutputStream) {
      new PrintStream(out, true)
    }

    @Override
    override def print(s: String) {
      //do what ever you like
      //      super.print(s)
      str += s
    }

    @Override
    override def println(s: String) {
      //do what ever you like
      //      super.print(s)
      str += s
    }

    def getTerminalOutput(): String = {
      str
    }
  }

  /**
   * Initializes all the streams to redirect the outputstream of the terminal.
   * @return
   */
  def initializeStreams(): (PrintStream, Interceptor) = {
    val origOut: PrintStream = System.out
    val interceptor: Interceptor = new Interceptor(origOut)
    System.setOut(interceptor); // just add the interceptor
    Console.setOut(interceptor)
    (origOut, interceptor)
  }

  /**
   * Redirects the outputstream of the console and runs the programm. The output
   * is finally returned as a string.
   * @return
   */
  def runProgramAndGetOutput(file: File = ctx.file): String = {
    val (origOut: PrintStream, interceptor: Interceptor) = initializeStreams()
    val program = pipeline.run(ctx)(file)
    while (program.hasNext) program.next
    System.setOut(origOut)
    Console.setOut(origOut)
    cleanString(interceptor.getTerminalOutput())
  }

  /**
   *
   * @return
   */
  private def readFileFromReferenceCompiler(file: File = ctx.file): String = {
    val proc: Process = Runtime.getRuntime().exec(commandForReferenceCompiler + file.getPath())
    proc.waitFor()
    // Then retrieve the process output
    val in: InputStream = proc.getInputStream()

    val b: Array[Byte] = new Array[Byte](in.available())
    in.read(b, 0, b.length)
    cleanString(new String(b))
  }

  /**
   * Clean given String from whitespaces and newlines so they are easier to compare
   * @param string
   * @return
   */
  def cleanString(string: String): String = string.replaceAll("^\\s+|\\s+$|\\s*(\n)\\s*|(\\s)\\s*", "")

  // ----------------------------------------------------------------------------------------------------------
  // ########################### Start of the actual tests  ###################################################
  // ----------------------------------------------------------------------------------------------------------

  "The Lexer" should "turn a ) to RPAREN token" in {
    val string = ")"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(RPAREN)

  }

  it should "turn a ( to LPAREN token" in {
    val string = "("
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(LPAREN)
  }

  it should "turn a : to COLON token" in {
    val string = ":"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(COLON)
  }

  it should "turn a ; to SEMICOLON token" in {
    val string = ";"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(SEMICOLON)
  }

  it should "turn a . to DOT token" in {
    val string = "."
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(DOT)
  }

  it should "turn a , to COMMA token" in {
    val string = ","
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(COMMA)
  }

  it should "turn a = to EQSIGN token" in {
    val string = "="
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(EQSIGN)
  }

  it should "turn a == to EQUALS token" in {
    val string = "=="
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(EQUALS)
  }

  it should "turn a ! to BANG token" in {
    val string = "!"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(BANG)
  }

  it should "turn a [ to LBRACKET token" in {
    val string = "["
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(LBRACKET)
  }

  it should "turn a ] to RBRACKET token" in {
    val string = "]"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(RBRACKET)
  }

  it should "turn a { to LBRACE token" in {
    val string = "{"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(LBRACE)
  }

  it should "turn a } to RBRACE token" in {
    val string = "}"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(RBRACE)
  }

  it should "turn a && to AND token" in {
    val string = "&&"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(AND)
  }

  it should "turn a || to OR token" in {
    val string = "||"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(OR)
  }

  it should "turn a < to LESSTHAN token" in {
    val string = "<"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(LESSTHAN)
  }

  it should "turn a + to PLUS token" in {
    val string = "+"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(PLUS)
  }

  it should "turn a - to MINUS token" in {
    val string = "-"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(MINUS)
  }

  it should "turn a * to TIMES token" in {
    val string = "*"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(TIMES)
  }

  it should "turn a / to DIV token" in {
    val string = "/"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(DIV)
  }

  it should "turn a object to OBJECT token" in {
    val string = "object"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(OBJECT)
  }

  it should "turn a class to CLASS token" in {
    val string = "class"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(CLASS)
  }

  it should "turn a def to DEF token" in {
    val string = "def"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(DEF)
  }

  it should "turn a var to VAR token" in {
    val string = "var"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(VAR)
  }

  it should "turn a Unit to UNIT token" in {
    val string = "Unit"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(UNIT)
  }

  it should "turn a main to MAIN token" in {
    val string = "main"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(MAIN)
  }

  it should "turn a String to STRING token" in {
    val string = "String"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(STRING)
  }

  it should "turn a extends to EXTENDS token" in {
    val string = "extends"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(EXTENDS)
  }

  it should "turn a Int to INT token" in {
    val string = "Int"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(INT)
  }

  it should "turn a Bool to BOOLEAN token" in {
    val string = "Bool"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(BOOLEAN)
  }

  it should "turn a while to WHILE token" in {
    val string = "while"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(WHILE)
  }

  it should "turn a if to IF token" in {
    val string = "if"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(IF)
  }

  it should "turn a else to ELSE token" in {
    val string = "else"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(ELSE)
  }

  it should "turn a return to RETURN token" in {
    val string = "return"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(RETURN)
  }

  it should "turn a length to LENGTH token" in {
    val string = "length"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(LENGTH)
  }

  it should "turn a true to TRUE token" in {
    val string = "true"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(TRUE)
  }

  it should "turn a false to FALSE token" in {
    val string = "false"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(FALSE)
  }

  it should "turn a this to THIS token" in {
    val string = "this"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(THIS)
  }

  it should "turn a new to NEW token" in {
    val string = "new"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(NEW)
  }

  it should "turn a println to PRINTLN token" in {
    val string = "println"
    setUpFile(string)

    val program = pipeline.run(ctx)(ctx.file)
    program.next().kind should be(PRINTLN)
  }

  // ########################### More complicated tests  ###########################################

  it should "lexer a complicated phrase correctly" in {
    val string =
      "(((\n// asdfasdfasdfasdpofiewfowbv ^&@$#^!#&@%\n\"Hello World!\" == \n))) x = 23 * 14 / 214 \n)))"
    setUpFile(string)
    runProgramAndGetOutput() should be(readFileFromReferenceCompiler())
  }

  it should "be possible to just write a number" in {
    val string =
      "123"
    setUpFile(string)
    runProgramAndGetOutput() should be(readFileFromReferenceCompiler())
  }

  it should "be possible to just write an identifier" in {
    val string =
      "asgaoasdfi"
    setUpFile(string)
    runProgramAndGetOutput() should be(readFileFromReferenceCompiler())
  }

  it should "check all kinds of bad signs" in {
    val string =
      "@$&\\#??|\'%€"
    setUpFile(string)
    val program = pipeline.run(ctx)(ctx.file)
    println(runProgramAndGetOutput())
    for( a <- 1 to 11) {
      program.next().kind should be(BAD)
    }
    program.next().kind should be(EOF)
  }

  def randomString(alphabet: String)(n: Int): String =
    Stream.continually(new Random().nextInt(alphabet.size)).map(alphabet).take(n).mkString

  def randomAlphanumericString(n: Int) =
    randomString(" \tabcdefghijklmnopqrstuvwxyz0123456789(){}[];:\\!@#$%^&|/\'\"*,.")(n)

  it should "pass a test of 100 random signs" in {
    val string =
      randomAlphanumericString(100)
    println(string)
    setUpFile(string)
    runProgramAndGetOutput() should be(readFileFromReferenceCompiler())
  }

  val lab2: File = new File("../philipp-test/lab2/valid/")
  for(f <- lab2.listFiles()) {
    it should f.getName() in {
      runProgramAndGetOutput(f) should be(readFileFromReferenceCompiler(f))
    }
  }

  val lab3: File = new File("../philipp-test/lab3/valid/")
  for(f <- lab3.listFiles()) {
    it should f.getName() in {
      runProgramAndGetOutput(f) should be(readFileFromReferenceCompiler(f))
    }
  }

  val ourTest: File = new File("../programs/lexer/ok/")
  for(f <- ourTest.listFiles()) {
    it should f.getName() in {
      runProgramAndGetOutput(f) should be(readFileFromReferenceCompiler(f))
    }
  }
}
