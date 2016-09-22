import java.io._
import java.util.concurrent.{Executors, ExecutorService}

import koolc.analyzer.NameAnalysis
import koolc.ast.{Printer, ASTPrinter, Parser}
import koolc.lexer.Lexer
import koolc.lexer.Tokens.RPAREN
import koolc.utils.{Context, Reporter}
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class TestNameAnalysis extends FlatSpec with Matchers with BeforeAndAfter {

  val reporter = new Reporter()
  var outDir: Option[File] = None
  var files: List[File] = Nil
  val ctx = Context(reporter = reporter, file = new File("temporaryFile"), outDir = outDir, printTokens = true)


  def run(file: File): String = {
    val pipeline = Lexer andThen Parser andThen NameAnalysis
    val program = pipeline.run(ctx)(file)
    Printer(program)
  }

  val validTestCases: File = new File("../programs/analyzer/ok/")
  for (f <- validTestCases.listFiles()) {
    it should f.getName() in {
      run(f)
    }
  }

  val badTestCases: File = new File("../programs/analyzer/bad/")
  var t: Thread = new Thread()
  for (f <- badTestCases.listFiles()) {
    it should f.getName() in {
      run(f)
    }
  }


}
