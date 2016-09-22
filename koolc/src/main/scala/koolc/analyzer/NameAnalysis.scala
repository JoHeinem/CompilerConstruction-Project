package koolc
package analyzer

import koolc.Main.main
import utils._
import ast.Trees._
import koolc.analyzer.Types._
import Symbols._

import scala.collection.mutable.ListBuffer

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check for all constraints

    /** Symbols of the global scope */
    val globalScope: GlobalScope = new GlobalScope

    def createSymbols: Unit = {
      analyseMainObject(prog.main)
      prog.classes.foreach(analyseClass)
    }

    def analyseMainObject(main: MainObject) = {
      val mainSym: ClassSymbol = new ClassSymbol(main.id.value)
      mainSym.setType(TObject(mainSym,None))
      mainSym.setPos(main)
      globalScope.classes += (main.id.value -> mainSym)
      globalScope.mainClass = mainSym
      main.setSymbol(mainSym)
      main.id.setSymbol(mainSym)
    }

    def analyseClass(classDecl: ClassDecl) = {
      val classSym = globalScope.lookupClass(classDecl.id.value) match {
        case Some(s) =>
          error("Object or class declaration already exists...", classDecl)
          error("...declared here", s)
          s
        case None => new ClassSymbol(classDecl.id.value)
      }
      classSym.setType(TObject(classSym, None))
      classSym.setPos(classDecl)
      classDecl.setSymbol(classSym)
      classDecl.id.setSymbol(classSym)
      handleGenerics(classDecl, classSym)
      globalScope.classes += (classDecl.id.value -> classSym)

      classDecl.methods.foreach(analyseMethod(_, classSym))
    }

    def handleGenerics(cd: ClassDecl, cs: ClassSymbol) = cd.id.genericType match {
      case Some(t) =>
        t match {
          case Identifier(v,_) =>
            var gs = new GenericSymbol(v)
            gs.setType(TGeneric(gs))
            gs.setPos(cd.id)
            cs.genericSymbol = Some(gs)
          case _ =>
        }
      case None =>
    }

    def addVarToClassSym(vr: VarDecl, sym: ClassSymbol) = {
      def createVariableIfNotAlreadyDeclared(classSymbol: ClassSymbol): VariableSymbol = {
        classSymbol.lookupVar(vr.id.value) match {
          case Some(s) =>
            error("Variable declaration already exists...", vr)
            error("...declared here", s)
            s
          case None =>
            classSymbol.parent match {
              case Some(parentClassSymbol) =>
                createVariableIfNotAlreadyDeclared(parentClassSymbol)
              case None => new VariableSymbol(vr.id.value)
            }
        }
      }

      val valSym: VariableSymbol = createVariableIfNotAlreadyDeclared(sym)
      valSym.setPos(vr)
      setTypeSymbol(vr.tpe, sym)
      valSym.setType(vr.tpe.getType)
      vr.setSymbol(valSym)
      vr.id.setSymbol(valSym)
      sym.members += (vr.id.value -> valSym)
    }

    def analyseMethod(methodDecl: MethodDecl, classSym: ClassSymbol) = {
      val methodSym = classSym.lookupMethod(methodDecl.id.value) match {
        case Some(s) => {
          error("Method declaration already exists...", methodDecl)
          error("...declared here", s)
          s
        } case None => new MethodSymbol(methodDecl.id.value, classSym)
      }
      methodSym.setPos(methodDecl)
      methodDecl.setSymbol(methodSym)
      methodDecl.id.setSymbol(methodSym)
      classSym.methods += (methodDecl.id.value -> methodSym)
    }

    def addVarToMethodSym(vr: VarDecl, sym: MethodSymbol, clss: ClassSymbol) = {
      val valSym = sym.lookupVar(vr.id.value) match {
        case Some(s) => {
          error("Variable declaration already exists...", vr)
          error("...declared here", s)
          s
        } case None => new VariableSymbol(vr.id.value)
      }
      valSym.setPos(vr)
      setTypeSymbol(vr.tpe, clss)
      valSym.setType(vr.tpe.getType)
      vr.setSymbol(valSym)
      vr.id.setSymbol(valSym)
      sym.members += (vr.id.value -> valSym)
    }

    def addFormalToMethod(formal: Formal, ms: MethodSymbol, cs: ClassSymbol, lb: ListBuffer[VariableSymbol]) = {
      val valSym = ms.lookupVar(formal.id.value) match {
        case Some(s) => {
          error("Variable declaration already exists...", formal)
          error("...declared here", s)
          s
        } case None => new VariableSymbol(formal.id.value)
      }
      valSym.setPos(formal)
      setTypeSymbol(formal.tpe, cs)
      valSym.setType(formal.tpe.getType)
      formal.setSymbol(valSym)
      formal.id.setSymbol(valSym)
      ms.params += (formal.id.value -> valSym)
      lb += valSym
    }

/* ------------------------
 * ------ sweep two -------
 * ------------------------
 */

    def attachSymbols: Unit = {
      findRefMO(prog.main)
      prog.classes.foreach(setParentClass)
      prog.classes.foreach(setClassMembers)
      prog.classes.foreach(findRefCD)
      prog.classes.foreach(findOverrides)
    }

    def findOverrides(clss: ClassDecl): Unit = {
      clss.methods.foreach(detectOverrides(_, clss.getSymbol))
    }

    def setParentClass(clss: ClassDecl): Unit = {
      clss.parent match {
        case Some(p) => {
          globalScope.lookupClass(p.value) match {
            case Some(c) => clss.getSymbol.parent = Option(c); p.setSymbol(c)
            case None => error("Class not defined", p)
          }
        } case None =>
      }
    }

    def setClassMembers(classDecl: ClassDecl): Unit = {
      classDecl.vars.foreach(addVarToClassSym(_, classDecl.getSymbol))
    }

    def findRefMO(main: MainObject): Unit = {
      main.stats.foreach(findRefStat(_))
    }

    def findRefCD(clss: ClassDecl): Unit = {
      detectCircleInheritance(clss.getSymbol)
      for (m <- clss.methods) findRefMeth(m, clss.getSymbol)
    }

    def setTypeSymbol(tpe: TypeTree, cs: ClassSymbol) {
      tpe match {
        case id: Identifier => {
          globalScope.lookupClass(id.value) match {
            case Some(s) =>
              if(s == globalScope.mainClass){
                error("Main object is not allowed as a variable type!", tpe)
              }
              id.setSymbol(s)
            case None =>
              cs.genericSymbol match {
                case Some(gs) =>
                  if (id.value == gs.name)
                    id.setSymbol(gs)
                case None =>
                  error("Class not declared", tpe)
              }
          }
        } case _ =>
      }
    }

    def findRefMeth(meth: MethodDecl, cs: ClassSymbol): Unit = {
      def setReturnTypeOfMethodSymbol: Unit = {
        cs.lookupMethod(meth.id.value) match {
          case Some(method) =>
            method.setType(meth.getSymbol.getType)
          case None =>
        }
      }

      var lb = new ListBuffer[VariableSymbol]
      meth.args.foreach(addFormalToMethod(_, meth.getSymbol, cs, lb))
      meth.getSymbol.argList = lb.toList
      meth.vars.foreach(addVarToMethodSym(_, meth.getSymbol, cs))
      meth.stats.foreach(findRefStat(_, cs, meth.getSymbol))
      findRefExpr(meth.retExpr, cs, meth.getSymbol)
      setTypeSymbol(meth.retType, cs)
      meth.getSymbol.setType(meth.retType.getType)
      setReturnTypeOfMethodSymbol
    }

    def detectOverrides(meth: MethodDecl, cs: ClassSymbol): Unit = {
      cs.parent match {
        case Some(c) => {
          c.lookupMethod(meth.id.value) match {
            case Some(m) =>
              var overrides: Boolean = true
              overrides = m.argList.size == meth.getSymbol.argList.size
              for (i <- 0 until m.argList.size)
                overrides = (m.argList(i).getType == meth.getSymbol.argList(i).getType) && overrides
              overrides = (m.getType == meth.getSymbol.getType) && overrides
              if (overrides) {
                meth.getSymbol.overridden = Some(m)
              } else {
                error("A method with that name already exists...", meth)
                error("...here", m)
              }
            case None =>
              detectOverrides(meth, c)
          }
        } case None =>
      }
    }

    def detectCircleInheritance(c: ClassSymbol): Unit = {
      var parent: Option[ClassSymbol] = c.parent
      var done = false
      while (!done) {
        parent match {
          case Some(s) => {
            if (c == s) {
              error("Circle inheritance", c)
              done = true
            } else if (s == globalScope.mainClass) {
              error("Class cannot extend the main object", c)
              done = true
            }
            parent = s.parent
          }
          case None => {
            done = true
          }
        }
      }
    }

    def findRefStat(stat: StatTree, clss: ClassSymbol = null, meth: MethodSymbol = null): Unit = stat match {
      case Block(stats) => stats.foreach(findRefStat(_, clss, meth))
      case iStat: If => findRefIf(iStat, clss, meth)
      case wStat: While => findRefWhile(wStat, clss, meth)
      case Println(expr) => findRefExpr(expr, clss, meth)
      case asg: Assign => findRefAsg(asg, clss, meth)
      case arasg: ArrayAssign => findRefArAsg(arasg, clss, meth)
    }

    def findRefIf(i: If, clss: ClassSymbol = null, meth: MethodSymbol = null): Unit = {
      findRefExpr(i.expr, clss, meth)
      findRefStat(i.thn, clss, meth)
      i.els match {
        case Some(stat) => findRefStat(stat, clss, meth)
        case None =>
      }
    }

    def findRefWhile(w: While, clss: ClassSymbol = null, meth: MethodSymbol = null): Unit = {
      findRefExpr(w.expr, clss, meth)
      findRefStat(w.stat, clss, meth)
    }

    def findRefAsg(a: Assign, clss: ClassSymbol = null, meth: MethodSymbol = null): Unit = {
      findRefIdGlobal(a.id, clss, meth)
      findRefExpr(a.expr, clss, meth)
    }

    def findRefArAsg(a: ArrayAssign, clss: ClassSymbol = null, meth: MethodSymbol = null): Unit = {
      findRefIdGlobal(a.id, clss, meth)
      findRefExpr(a.index, clss, meth)
      findRefExpr(a.expr, clss, meth)
    }

    def findRefId(id: Identifier, clss: ClassSymbol = null, meth: MethodSymbol = null): Unit = {
      var found = false
      if (clss != null) {
      clss.lookupVar(id.value) match {
        case Some(s) => found=true; s.accessed=true; id.setSymbol(s)
        case None =>
      }
      }
      if (meth != null) {
        meth.lookupVar(id.value) match {
          case Some(s) => found=true; s.accessed=true; id.setSymbol(s)
          case None =>
        }
      }
      if (!found) error("Identifier not declared", id)
    }

    def findRefIdGlobal(id: Identifier, clss: ClassSymbol = null, meth: MethodSymbol = null): Unit = {
      id.genericType match {
        case Some(gen) => 
          gen match {
            case genid: Identifier => findRefIdGlobal(genid,clss,meth)
            case _ =>
          }
        case None =>
      }
      var found = false
      globalScope.lookupClass(id.value) match {
        case Some(s) => found=true; s.accessed=true; id.setSymbol(s)
        case None =>
      }
      if (clss != null) {
      clss.lookupVar(id.value) match {
        case Some(s) => found=true; s.accessed=true; id.setSymbol(s)
        case None =>
      }
      }
      if (meth != null) {
        meth.lookupVar(id.value) match {
          case Some(s) => found=true; s.accessed=true; id.setSymbol(s)
          case None =>
        }
      }
      if (!found) error("Class or identifier not declared", id)
    }

    def findRefExpr(expr: ExprTree, cs: ClassSymbol = null, ms: MethodSymbol = null): Unit = expr match {
      case And(lhs,rhs) => findRefExpr(lhs, cs, ms); findRefExpr(rhs, cs, ms);
      case Or(lhs,rhs) => findRefExpr(lhs, cs, ms); findRefExpr(rhs, cs, ms);
      case Plus(lhs,rhs) => findRefExpr(lhs, cs, ms); findRefExpr(rhs, cs, ms);
      case Minus(lhs,rhs) => findRefExpr(lhs, cs, ms); findRefExpr(rhs, cs, ms);
      case Times(lhs,rhs) => findRefExpr(lhs, cs, ms); findRefExpr(rhs, cs, ms);
      case Div(lhs,rhs) => findRefExpr(lhs, cs, ms); findRefExpr(rhs, cs, ms);
      case LessThan(lhs,rhs) => findRefExpr(lhs, cs, ms); findRefExpr(rhs, cs, ms);
      case Equals(lhs,rhs) => findRefExpr(lhs, cs, ms); findRefExpr(rhs, cs, ms);
      case ArrayRead(arr, ind) => findRefExpr(arr, cs, ms); findRefExpr(ind, cs, ms);
      case ArrayLength(arr) => findRefExpr(arr, cs, ms);
      case MethodCall(ob, me, ar) => findRefExpr(ob, cs, ms);ar.foreach(findRefExpr(_, cs, ms));
      case id: Identifier => findRefId(id, cs, ms)
      case IntLit(_) | StringLit(_) | True() | False() =>
      case t: This => t.setSymbol(cs); t.setType(cs.getType)
      case NewIntArray(size) => findRefExpr(size, cs, ms)
      case New(tpe) => findRefIdGlobal(tpe, cs, ms)
      case Not(e) => findRefExpr(e, cs, ms)
    }

    def uselessVariables(): Unit = {
      uselessClass(globalScope.mainClass)
      for ((_,c) <- globalScope.classes) uselessClass(c)
    }

    def uselessClass(clss: ClassSymbol): Unit = {
      for ((_,m) <- clss.methods) uselessMethod(m)
      for ((_,s) <- clss.members) checkAccess(s)
    }

    def uselessMethod(meth: MethodSymbol): Unit = {
      for ((_,s) <- meth.params) checkAccess(s)
      for ((_,s) <- meth.members) checkAccess(s)
    }

    def checkAccess(s: Symbol): Unit = {
      if (!s.accessed)
        warning("Declared but never used", s)
    }

    createSymbols
    attachSymbols
    uselessVariables
    terminateIfErrors
    prog
  }
}
