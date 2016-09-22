package koolc

import utils._
import java.io.File
import java.io.FileOutputStream // REMOVE LATER

import lexer._
import ast._
import analyzer._
import code._

object Main {

  def processOptions(args: Array[String]): Context = {

    val reporter = new Reporter()
    var outDir: Option[File] = None
    var files: List[File] = Nil
    var printTokens: Boolean = false

    def processOption(args: List[String]): Unit = args match {
      case "-d" :: out :: args =>
        outDir = Some(new File(out))
        processOption(args)

      case "--tokens":: args =>
        printTokens = true
        processOption(args)

      case f ::args =>
        files = new File(f) :: files
        processOption(args)

      case Nil =>
    }

    processOption(args.toList)

    if (files.size != 1) {
      reporter.fatal("Exactly one file expected, "+files.size+" file(s) given.")
    }

    Context(reporter = reporter, file = files.head, outDir = outDir, printTokens = printTokens)
  }

  def main(args: Array[String]) {
    val ctx = processOptions(args)
    if (ctx.printTokens) {
      val pipeline = Lexer andThen PrintTokens
      val program = pipeline.run(ctx)(ctx.file)
      while (program.hasNext) program.next
    } else {
      val pipeline = Lexer andThen
        Parser andThen
        NameAnalysis andThen
        TypeChecking andThen
        CodeGeneration
      pipeline.run(ctx)(ctx.file)
    }
  }
}
