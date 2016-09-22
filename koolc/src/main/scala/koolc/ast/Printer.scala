package koolc
package ast

import Trees._
import analyzer.Symbols._
import scala.collection.mutable.StringBuilder

object Printer {
  def apply(t: Tree): String = {
    var sb = new StringBuilder()
    t match {
      case Program(mainObj, classDecl) =>
        sb.append(printMainObject(mainObj)).append("\n")
          .append(classDecl.map(printClassDecl(_)).mkString("\n"))
          .append("\n").toString
      case _ => "Not instance of program"
    }
  }

  def printMainObject(mo: MainObject): String = {
    var sb = new StringBuilder()
    sb.append("object ")
      .append(printIdentifier(mo.id))
      .append(" {\ndef main(): Unit = {\n")
      .append(mo.stats.map(printStatement(_)).mkString("\n"))
      .append("\n}\n}").toString

  }

  def printClassDecl(clss: ClassDecl): String = {
    var sb = new StringBuilder()
    sb.append("class ")
      .append(printIdentifier(clss.id))
    clss.parent match {
      case Some(iden) => sb.append(" extends ").append(printIdentifier(iden))
      case None =>
    }
    sb.append(" {\n")
      .append(clss.vars.map(printVarDecl(_)).mkString("\n"))
      .append("\n")
      .append(clss.methods.map(printMethodDecl(_)).mkString("\n"))
      .append("\n}").toString
  }

  def printVarDecl(vr: VarDecl): String = {
    var sb = new StringBuilder()
    sb.append("var ")
      .append(printIdentifier(vr.id))
      .append(": ")
      .append(printType(vr.tpe)).append(";").toString
  }

  def printMethodDecl(method: MethodDecl): String = {
    var sb = new StringBuilder()
    sb.append("def ").append(printIdentifier(method.id))
      .append("(").append(method.args.map(printFormal(_)).mkString(", "))
      .append("): ").append(printType(method.retType)).append(" = {\n")
      .append(method.vars.map(printVarDecl(_)).mkString("\n"))
      .append("\n")
      .append(method.stats.map(printStatement(_)).mkString("\n"))
      .append("\nreturn ").append(printExpression(method.retExpr))
      .append(";\n}").toString
  }

  def printIdentifier(id: Identifier): String = {
    var sb = new StringBuilder()
    sb.append(id.value).append(idToString(id.getSymbol.id)).toString
  }

  def printType(tpe: TypeTree): String = tpe match {
    case IntArrayType() => "Int[]"
    case IntType() => "Int"
    case BooleanType() => "Bool"
    case StringType() => "String"
    case id: Identifier => printIdentifier(id)
  }

  def printFormal(formal: Formal): String = {
    var sb = new StringBuilder()
    sb.append(printIdentifier(formal.id)).append(": ")
      .append(printType(formal.tpe)).toString
  }

  def printStatement(stmt: StatTree): String = stmt match {
    case Block(stats) => printBlock(stats)
    case If(expr, thn, els) => printIf(expr, thn, els)
    case While(expr, stat) => printWhile(expr, stat)
    case Println(expr) => printPrintln(expr)
    case a: Assign => printAssign(a)
    case aa: ArrayAssign => printArrayAssign(aa)
  }

  def printBlock(stats: List[StatTree]): String = {
    var sb = new StringBuilder()
    sb.append("{\n")
      .append(stats.map(printStatement(_)).mkString("\n"))
      .append("\n}").toString
  }

  def printIf(expr: ExprTree, thn: StatTree, els: Option[StatTree]): String = {
    var sb = new StringBuilder()
    sb.append("if (").append(printExpression(expr))
      .append(")\n").append(printStatement(thn))
    els match {
      case Some(stat) => sb.append(" else ").append(printStatement(stat))
      case None =>
    }
    sb.toString
  }

  def printWhile(expr: ExprTree, stat: StatTree): String = {
    var sb = new StringBuilder()
    sb.append("while (").append(printExpression(expr))
      .append(")\n").append(printStatement(stat)).toString
  }

  def printPrintln(expr: ExprTree): String = {
    var sb = new StringBuilder()
    sb.append("println(")
      .append(printExpression(expr))
      .append(");").toString
  }

  def printAssign(asg: Assign): String = {
    var sb = new StringBuilder()
    sb.append(printIdentifier(asg.id))
      .append(" = ")
      .append(printExpression(asg.expr))
      .append(";").toString
  }

  def printArrayAssign(asg: ArrayAssign): String = {
    var sb = new StringBuilder()
    sb.append(printIdentifier(asg.id))
      .append("[")
      .append(printExpression(asg.index))
      .append("] = ")
      .append(printExpression(asg.expr))
      .append(";").toString
  }

  def printExpression(expr: ExprTree): String = expr match {
    case And(lhs, rhs) => printBinaryOperator(lhs, rhs, " && ")
    case Or(lhs, rhs) => printBinaryOperator(lhs, rhs, " || ")
    case Plus(lhs, rhs) => printBinaryOperator(lhs, rhs, " + ")
    case Minus(lhs, rhs) => printBinaryOperator(lhs, rhs, " - ")
    case Times(lhs, rhs) => printBinaryOperator(lhs, rhs, " * ")
    case Div(lhs, rhs) => printBinaryOperator(lhs, rhs, " / ")
    case LessThan(lhs, rhs) => printBinaryOperator(lhs, rhs, " < ")
    case Equals(lhs, rhs) => printBinaryOperator(lhs, rhs, " == ")
    case IntLit(int) => int + ""
    case StringLit(str) => "\"" + str + "\""
    case True() => "true"
    case False() => "false"
    case t: This => "this" + idToString(t.getSymbol.id)
    case Not(expr) => "!" + printExpression(expr)
    case ArrayRead(arr, index) => printArrayRead(arr, index)
    case ArrayLength(arr) => printExpression(arr) + ".length"
    case MethodCall(obj, meth, args) => printMethodCall(obj, meth, args)
    case id: Identifier => printIdentifier(id)
    case NewIntArray(size) => "new Int[" + printExpression(size) + "]"
    case New(id) => "new " + printIdentifier(id) + "()"
  }

  def printMethodCall(obj: ExprTree, meth: Identifier, args: List[ExprTree]): String = {
    var sb = new StringBuilder()
    sb.append(printExpression(obj))
      .append(".")
      .append(printIdentifier(meth))
      .append("(")
      .append(args.map(printExpression(_)).mkString(", "))
      .append(")").toString
  }

  def printArrayRead(arr: ExprTree, index: ExprTree): String = {
    var sb = new StringBuilder()
    sb.append(printExpression(arr))
      .append("[")
      .append(printExpression(index))
      .append("]").toString
  }

  def printBinaryOperator(lhs: ExprTree, rhs: ExprTree, op: String): String = {
    var sb = new StringBuilder()
    var thisPrio = priority(op)
    var lhsPrio = priority(exprToOp(lhs))
    var rhsPrio = priority(exprToOp(rhs))
    if (thisPrio > lhsPrio) sb.append("(")
    sb.append(printExpression(lhs))
    if (thisPrio > lhsPrio) sb.append(")")
    sb.append(op)
    if (thisPrio > rhsPrio) sb.append("(")
    sb.append(printExpression(rhs))
    if (thisPrio > rhsPrio) sb.append(")")
    sb.toString
  }

  def exprToOp(expr: ExprTree): String = expr match {
    case And(_,_) => " && "
    case Or(_,_) => " || "
    case Plus(_,_) => " + "
    case Minus(_,_) => " - "
    case Times(_,_) => " * "
    case Div(_,_) => " / "
    case LessThan(_,_) => " < "
    case Equals(_,_) => " == "
    case _ => "expr"
  }

  def priority(op: String): Int = op match {
    case "expr" => 9
    case " * " => 8
    case " / " => 7
    case " + " => 6
    case " - " => 5
    case " < " => 4
    case " == " => 3
    case " && " => 2
    case " || " => 1
  }

  def idToString(num: Int): String = "#" + num
}
