import java.io.{FileOutputStream, PrintStream, File, OutputStream, InputStream}

import koolc.ast.{Parser, ASTPrinter}
import koolc.lexer.Tokens._
import koolc.lexer.{PrintTokens, Token, Lexer}
import koolc.utils.{Reporter, Context, Pipeline}
import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

import scala.util.Random

class TestAST extends FlatSpec with Matchers with BeforeAndAfter {

  val commandForReferenceCompiler: String =
    "scala -cp ../koolc_2.11-1.2.jar:../cafebabe_2.11-1.2.jar koolc.Main --ast "
  var ctx: Context = null

  before {
    ctx = processOptions(new Array[String](0))
  }

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

    Context(reporter = reporter, file = null, outDir = outDir, printTokens = true)
  }

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

  def initializeStreams(): (PrintStream, Interceptor) = {
    val origOut: PrintStream = System.out
    val interceptor: Interceptor = new Interceptor(origOut)
    System.setOut(interceptor); // just add the interceptor
    Console.setOut(interceptor)
    (origOut, interceptor)
  }

  def runProgramAndGetOutput(file: File): String = {
    val pipeline = Lexer andThen Parser
    val program = pipeline.run(ctx)(file)
    clean(ASTPrinter(program))
  }

  private def readFileFromReferenceCompiler(file: File): String = {
    val proc: Process = Runtime.getRuntime().exec(commandForReferenceCompiler + file.getPath())
    proc.waitFor()
    // Then retrieve the process output
    val in: InputStream = proc.getInputStream()

    val b: Array[Byte] = new Array[Byte](in.available())
    in.read(b, 0, b.length)
    clean(new String(b))
  }

  def clean(string: String): String = string.replaceAll("^\\s+|\\s+$|\\s*(\n)\\s*|(\\s)\\s*", "")

  val lab3: File = new File("../philipp-test/lab3/valid/")
  for(f <- lab3.listFiles()) {
    it should f.getName() in {
      runProgramAndGetOutput(f) should be(readFileFromReferenceCompiler(f))
    }
  }

  val ourTest: File = new File("../programs/parser/ok/")
  for(f <- ourTest.listFiles()) {
    it should f.getName() in {
      runProgramAndGetOutput(f) should be(readFileFromReferenceCompiler(f))
    }
  }
}
