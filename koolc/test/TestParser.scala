import java.io.{FileOutputStream, PrintStream, File, OutputStream, InputStream}

import koolc.ast.{Printer, Parser}
import koolc.lexer.Tokens._
import koolc.lexer.{PrintTokens, Token, Lexer}
import koolc.utils.{Reporter, Context, Pipeline}
import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

class TestParser extends FlatSpec with Matchers with BeforeAndAfter {

  val reporter = new Reporter()
  var outDir: Option[File] = None
  var files: List[File] = Nil
  val ctx = Context(reporter = reporter, file = new File("temporaryFile"), outDir = outDir, printTokens = true)

  def stringToFile(str: String): File = {
    val file = new File("temporaryFile")
    val fos = new FileOutputStream(file)
    fos.write(str.getBytes)
    fos.flush()
    fos.close()
    file
  }

  def run(file: File): String = {
    val pipeline = Lexer andThen Parser
    val program = pipeline.run(ctx)(file)
    Printer(program)
  }

  val lab3: File = new File("../philipp-test/lab3/valid/")
  for(f <- lab3.listFiles()) {
    it should f.getName() in {
      val str = run(f)
      str should be(run(stringToFile(str)))
    }
  }

  val own: File = new File("../programs/parser/ok/")
  for(f <- own.listFiles()) {
    it should f.getName() in {
      val str = run(f)
      str should be(run(stringToFile(str)))
    }
  }
}
