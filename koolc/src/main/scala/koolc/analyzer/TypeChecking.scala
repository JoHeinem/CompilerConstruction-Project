package koolc
package analyzer

import koolc.analyzer
import koolc.ast

import ast.Trees._

import Symbols._
import Types._
import utils._

object TypeChecking extends Pipeline[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
    * attaching types to trees and potentially outputting error messages. */
  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def typeCheck(prog: Program): Unit = {
      prog.main.stats.foreach(tcStat)
      prog.classes.foreach(tcClass)
    }

    def tcClass(cl: ClassDecl): Unit = {
      cl.methods.foreach(tcMeth)
    }

    def tcMeth(m: MethodDecl): Unit = {
      m.stats.foreach(tcStat)
      m.retExpr.setType(tcExpr(m.retExpr, m.getSymbol.getType));
    }

    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      var generic = false
      val tpe: Type = expr match {
        case And(lhs, rhs) => lhs.setType(tcExpr(lhs, TBoolean)); rhs.setType(tcExpr(rhs, TBoolean)); TBoolean
        case Or(lhs, rhs) => lhs.setType(tcExpr(lhs, TBoolean)); rhs.setType(tcExpr(rhs, TBoolean)); TBoolean
        case p: Plus => tcPlus(p)
        case Minus(lhs, rhs) => lhs.setType(tcExpr(lhs, TInt)); rhs.setType(tcExpr(rhs, TInt)); TInt
        case Times(lhs, rhs) => lhs.setType(tcExpr(lhs, TInt)); rhs.setType(tcExpr(rhs, TInt)); TInt
        case Div(lhs, rhs) => lhs.setType(tcExpr(lhs, TInt)); rhs.setType(tcExpr(rhs, TInt)); TInt
        case LessThan(lhs, rhs) => lhs.setType(tcExpr(lhs, TInt)); rhs.setType(tcExpr(rhs, TInt)); TBoolean
        case e: Equals => tcEquals(e)
        case ArrayRead(ar, in) => ar.setType(tcExpr(ar, TIntArray)); in.setType(tcExpr(in, TInt)); TInt
        case ArrayLength(arr) => arr.setType(tcExpr(arr, TIntArray)); TInt
        case mc: MethodCall =>
          mc.setType(tcMethCall(mc))
          mc.obj.getType match {
            case TObject(cs,Some(t)) =>
              generic = true
              if (!expected.isEmpty && !expected.contains(typeTreeToType(t,cs)))
                error("Type error", mc)
            case _ =>
          }
          mc.getType
        case IntLit(_) => TInt
        case StringLit(_) => TString
        case t: This => TObject(t.getSymbol, None)
        case NewIntArray(sz) => sz.setType(tcExpr(sz, TInt)); TIntArray
        case New(tpe) => tpe.getType
        case Not(e) => e.setType(tcExpr(e, TBoolean)); TBoolean
        case id: Identifier => id.getType
        case True() | False() => TBoolean
      }

      if (generic) return tpe
      if (expected.isEmpty) {
        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
        expected.head
      } else {
        tpe
      }
    }

    def typeTreeToType(tt: TypeTree, cs: ClassSymbol): Type = tt match {
      case StringType() => TString
      case BooleanType() => TBoolean
      case IntArrayType() => TIntArray
      case IntType() => TInt
      case Identifier(v,_) => TObject(cs,None)
    }

    def tcMethCall(mc: MethodCall): Type = {
      mc.obj.getType match {
        case TObject(c,_) =>
          c.lookupMethod(mc.meth.value) match {
            case Some(m) =>
            case None =>
              c.parent match {
                case Some(p) =>
                  p.lookupMethod(mc.meth.value) match {
                    case Some(m) =>
                    case None => error("There is no method with that name", mc); return TUntyped
                  }
                case None => error("There is no method with that name", mc); return TUntyped
              }
          }
        case _ =>
      }
      for (a <- mc.args) a.setType(tcExpr(a))
      def getTypeOfMethod(classSymbol: ClassSymbol): Type = {
        classSymbol.lookupMethod(mc.meth.value) match {
          case Some(methodSymbol) =>
            mc.meth.setSymbol(methodSymbol)
            methodSymbol.getType
          case None =>
            classSymbol.parent match {
              case Some(parentClass) =>
                getTypeOfMethod(parentClass)
              case None => TError
            }
        }
      }

      var ret: Type = TError
      mc.obj.setType(tcExpr(mc.obj))
      mc.obj.getType match {
        case TObject(classSymbol,_) => {
          ret = getTypeOfMethod(classSymbol)
        }
        case _ => ret = TError
      }
      checkArgTypes(mc)
      ret
    }

    def checkArgTypes(mc: MethodCall): Unit = {
      val a: List[Type] = mc.meth.getSymbol match {
        case m: MethodSymbol => m.argList.map(_.getType)
        case _ => Nil
      }
      val b: List[Type] = mc.args.map(_.getType)
      if (a.size != b.size) {
        error(mc.meth.value + " takes " + a.size + " arguments, " +
          b.size + " provided", mc);
      }
      var i: Int = 0
      for (i <- 0 until math.min(a.size,b.size)) {
        if (a(i).toString == "generic") {
          mc.obj.getType match {
            case TObject(_,Some(t)) =>
              if(mc.args(i).getType != t.getType) {
                var err: String = "Type error: expected: " +
                  t.getType + " found: " + mc.args(i).getType
                error(err, mc.args(i))
              }
            case _ =>
          }
        } else {
          mc.args(i).setType(tcExpr(mc.args(i), a(i)))
        }
      }
    }

    def tcPlus(p: Plus): Type = {
      p.lhs.setType(tcExpr(p.lhs, TString, TInt))
      p.rhs.setType(tcExpr(p.rhs, TString, TInt))
      if (p.lhs.getType == TInt && p.rhs.getType == TInt) TInt
      else if (p.lhs.getType == TInt && p.rhs.getType == TString) TString
      else if (p.lhs.getType == TString && p.rhs.getType == TInt) TString
      else if (p.lhs.getType == TString && p.rhs.getType == TString) TString
      else if (p.lhs.getType.toString == "generic" || p.rhs.getType.toString == "generic") {
        TString
      } else {
        error("Cannot use + on " + p.lhs.getType + " and " + p.rhs.getType, p)
        TError
      }
    }

    def tcEquals(e: Equals): Type = {
      e.lhs.setType(tcExpr(e.lhs))
      e.rhs.setType(tcExpr(e.rhs))
      e.lhs.getType match {
        case TInt => if (e.rhs.getType == TInt) return TBoolean
        case TString => if (e.rhs.getType == TString) return TBoolean
        case TIntArray => if (e.rhs.getType == TIntArray) return TBoolean
        case TBoolean => if (e.rhs.getType == TBoolean) return TBoolean
        case TObject(_,_) =>
          e.rhs.getType match {
            case TObject(_,_) => return TBoolean
            case _ =>
          }
        case TGeneric(_) =>
          e.rhs.getType match {
            case TGeneric(_) => return TBoolean
            case _ =>
          }
        case _ =>
      }
      error("Cannot compare " + e.lhs.getType + " and " + e.rhs.getType, e)
      TError
    }

    def tcStat(stat: StatTree): Unit = stat match {
      case Block(stats) => stats.foreach(tcStat)
      case If(e, th, els) => {
        e.setType(tcExpr(e, TBoolean))
        tcStat(th)
        els match {
          case Some(s) => tcStat(s)
          case None =>
        }
      }
      case While(e, stat) => {
        e.setType(tcExpr(e, TBoolean))
        tcStat(stat)
      }
      case Println(e) => e.setType(tcExpr(e, TInt, TString, TBoolean))
      case Assign(id, e) => e.setType(tcExpr(e, id.getType))
      case ArrayAssign(id, in, e) => {
        e.setType(tcExpr(e, TInt))
        in.setType(tcExpr(in, TInt))
      }
    }

    typeCheck(prog)
    terminateIfErrors
    prog
  }

}
