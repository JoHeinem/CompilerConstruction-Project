package koolc
package code

import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._
import utils._

import collection.mutable.HashMap

object CodeGeneration extends Pipeline[Program, Unit] {

  def run(ctx: Context)(prog: Program): Unit = {
    import ctx.reporter._

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {
      val classFile = ct.parent match {
        case Some(p) => new ClassFile(ct.id.value, Some(p.value))
        case None => new ClassFile(ct.id.value, None)
      }
      classFile.addDefaultConstructor
      classFile.setSourceFile(sourceName)

      ct.vars.foreach(ft => classFile.addField(typeToByteCode(ft.tpe.getType), ft.id.value))
      for (m <- ct.methods) {
        val ret = typeToByteCode(m.retType.getType)
        val name = m.id.value
        val args = m.args.map(f => typeToByteCode(f.tpe.getType))
        val ch = classFile.addMethod(ret, name, args).codeHandler
        generateMethodCode(ch, ct, m)
      }
      classFile.writeToFile(dir + ct.id.value + ".class")
    }

    def typeToByteCode(tpe: Type): String = tpe match {
      case TString => "Ljava/lang/String;"
      case TInt => "I"
      case TBoolean => "Z"
      case TIntArray => "[I"
      case TObject(v,_) => "L" + v.name + ";"
      case TGeneric(_) => "Ljava/lang/Object;";
      case TError | TUntyped => ""
    }

    def generateMainMethodCode(mo: MainObject, src: String, dir: String): Unit = {
      val classFile = new ClassFile(mo.id.value, None)
      classFile.setSourceFile(mo.id.value)
      classFile.addDefaultConstructor
      val ch = classFile.addMainMethod.codeHandler
      mo.stats.foreach(s => compileStmt(s,ch))
      ch << RETURN
      ch.freeze

      classFile.writeToFile(dir + mo.id.value + ".class")
    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, ct: ClassDecl, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol
      val methMap = HashMap[String,Int]();

      var i: Int = 1
      for(v: VariableSymbol <- mt.getSymbol.argList) {
        methMap += v.name -> i
        i += 1
      }
      for((s: String,v: VariableSymbol) <- mt.getSymbol.members)
        methMap += s -> ch.getFreshVar

      mt.stats.foreach(m => compileStmt(m,ch,ct.getSymbol,methMap))

      compile(mt.retExpr, ch,ct.getSymbol,methMap)
      methSym.getType match {
        case TInt | TBoolean => ch << IRETURN
        case _ => ch << ARETURN
      }
      ch.freeze
    }

    def compileStmt(s: StatTree, ch: CodeHandler, cs: ClassSymbol=null, map: HashMap[String,Int]=null): Unit = {
      ch << LineNumber(s.line)
      s match {
        case Block(ss) => ss.foreach(compileStmt(_,ch,cs,map))
        case i: If => compileIf(i, ch,cs,map)
        case w: While => compileWhile(w, ch,cs,map)
        case p: Println => compilePrintln(p, ch,cs,map)
        case a: Assign => handleAsg(a, ch, cs, map)
        case aa: ArrayAssign => handleArAsg(aa,ch,cs,map)
      }
    }

    def handleArAsg(aa: ArrayAssign, ch: CodeHandler, cs: ClassSymbol, map: HashMap[String,Int]): Unit = {
      map.get(aa.id.value) match {
        case Some(i) =>
          ch << ALoad(i)
          compile(aa.index,ch,cs,map)
          compile(aa.expr,ch,cs,map)
          ch << IASTORE
        case None =>
          cs.lookupVar(aa.id.value) match {
            case Some(v) =>
              ch <<
                ALoad(0) <<
                GetField(cs.name, aa.id.value, "[I")
              compile(aa.index,ch,cs,map)
              compile(aa.expr,ch,cs,map)
              ch << IASTORE
            case None =>
          }
      }
    }

    def handleAsg(a: Assign, ch: CodeHandler, cs: ClassSymbol, map: HashMap[String,Int]): Unit = {
      map.get(a.id.value) match {
        case Some(i) =>
          compile(a.expr,ch,cs,map)
          a.expr.getType match {
            case TInt | TBoolean => ch << IStore(i)
            case _ => ch << AStore(i)
          }
        case None =>
          ch << ALoad(0)
          compile(a.expr,ch,cs,map)
          cs.lookupVar(a.id.value) match {
            case Some(v) => ch << PutField(cs.name, a.id.value, typeToByteCode(v.getType))
            case None =>
          }
      }
    }

    def compileIf(i: If, ch: CodeHandler, cs: ClassSymbol, map: HashMap[String,Int]): Unit = {
      val labelElse = ch.getFreshLabel("ielse")
      val labelAfter = ch.getFreshLabel("iafter")
      compile(i.expr,ch,cs,map)
      ch << IfEq(labelElse)
      compileStmt(i.thn,ch,cs,map)
      ch << Goto(labelAfter) <<
            Label(labelElse)
      i.els match {
        case Some(es) => compileStmt(es,ch,cs,map)
        case None =>
      }
      ch << Label(labelAfter)
    }

    def compileWhile(w: While, ch: CodeHandler, cs: ClassSymbol, map: HashMap[String,Int]): Unit = {
      val labelLoop = ch.getFreshLabel("wloop")
      val labelAfter = ch.getFreshLabel("wafter")
      ch << Label(labelLoop)
      compile(w.expr,ch,cs,map)
      ch << IfEq(labelAfter)
      compileStmt(w.stat,ch,cs,map)
      ch << Goto(labelLoop) <<
            Label(labelAfter)
    }

    def compilePrintln(p: Println, ch: CodeHandler, cs: ClassSymbol, map: HashMap[String,Int]): Unit = {
      ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
      compile(p.expr,ch,cs,map)
      val desc = "(" + typeToByteCode(p.expr.getType) + ")V"
      ch << InvokeVirtual("java/io/PrintStream","println",desc)
    }

    def compile(e: ExprTree, ch: CodeHandler, cs: ClassSymbol, map: HashMap[String,Int]): Unit = e match {
      case Minus(e1,e2) => compile(e1,ch,cs,map); compile(e2,ch,cs,map); ch << ISUB
      case Times(e1,e2) => compile(e1,ch,cs,map); compile(e2,ch,cs,map); ch << IMUL
      case Div(e1,e2) => compile(e1,ch,cs,map); compile(e2,ch,cs,map); ch << IDIV
      case LessThan(e1,e2) => compileLessThan(e1,e2,ch,cs,map)
      case And(lhs, rhs) => compileAnd(lhs,rhs,ch,cs,map)
      case Or(lhs, rhs) => compileOr(lhs,rhs,ch,cs,map)
      case Plus(e1,e2) => compilePlus(e1,e2,ch,cs,map)
      case Not(expr) => compileNot(expr,ch,cs,map)
      case StringLit(s) => ch << Ldc(s)
      case IntLit(i) => ch << Ldc(i)
      case False() => ch << Ldc(0)
      case True() => ch << Ldc(1)
      case NewIntArray(size) => compile(size,ch,cs,map); ch << NewArray(10)
      case ArrayLength(arr) => compile(arr,ch,cs,map); ch << ARRAYLENGTH
      case e: Equals => compileEquals(e,ch,cs,map)
      case New(tpe) => ch << DefaultNew(tpe.value)
      case m: MethodCall => compileMethCall(m,ch,cs,map)
      case This() => ch << ALoad(0)
      case id: Identifier => compileId(id, ch, cs, map)
      case ArrayRead(a,i) => compile(a,ch,cs,map); compile(i,ch,cs,map); ch << IALOAD
    }

    def compileId(id: Identifier, ch: CodeHandler, cs: ClassSymbol, map: HashMap[String,Int]): Unit = {
      map.get(id.value) match {
        case Some(i) =>
          id.getType match {
            case TInt | TBoolean => ch << ILoad(i)
            case _ => ch << ALoad(i)
          }
        case None =>
          cs.lookupVar(id.value) match {
            case Some(v) => ch << ALoad(0) << GetField(cs.name, id.value, typeToByteCode(id.getType))
            case None =>
          }
      }

    }

    def compileMethCall(m: MethodCall, ch: CodeHandler, cs: ClassSymbol, map: HashMap[String,Int]): Unit = {
      compile(m.obj,ch,cs,map)
      m.args.foreach(a => compile(a,ch,cs,map))
      var desc: String = "";
      m.obj.getType match {
        case TObject(c,_) =>
          c.lookupMethod(m.meth.value) match {
            case Some(ms) =>
              desc = "(" +
                ms.argList.map(a => typeToByteCode(a.getType)).mkString("") +
                ")" + typeToByteCode(m.getType)
            case _ =>
              c.parent match {
                case Some(p) =>
                  p.lookupMethod(m.meth.value) match {
                    case Some(ms) =>
                      desc = "(" +
                        ms.argList.map(a => typeToByteCode(a.getType)).mkString("") +
                        ")" + typeToByteCode(m.getType)
                    case None =>
                  }
                case None =>
              }
          }
        case _ =>
      }
      ch << InvokeVirtual(m.obj.getType.toString, m.meth.value, desc)
    }

    def compileEquals(e: Equals, ch: CodeHandler, cs: ClassSymbol, map: HashMap[String,Int]): Unit = {
      compile(e.lhs,ch,cs,map)
      compile(e.rhs,ch,cs,map)
      val la = ch.getFreshLabel("la")
      val lb = ch.getFreshLabel("lb")
      e.lhs.getType match {
        case TInt | TBoolean => ch << If_ICmpEq(la)
        case _ => ch << If_ACmpEq(la)
      }
      ch <<
        Ldc(0) <<
        Goto(lb) <<
        Label(la) <<
        Ldc(1) <<
        Label(lb)
    }

    def compileNot(e: ExprTree, ch: CodeHandler, cs: ClassSymbol, map: HashMap[String,Int]): Unit = {
      val labelA = ch.getFreshLabel("labelA")
      val labelB = ch.getFreshLabel("labelB")
      compile(e,ch,cs,map)
      ch <<
        IfEq(labelA) <<
        Ldc(0) <<
        Goto(labelB) <<
        Label(labelA) <<
        Ldc(1) <<
        Label(labelB)
    }

    def compileAnd(lhs: ExprTree, rhs: ExprTree, ch: CodeHandler, cs: ClassSymbol, map: HashMap[String,Int]): Unit = {
      val label0 = ch.getFreshLabel("label0")
      val labele = ch.getFreshLabel("labele")
      compile(lhs,ch,cs,map)
      ch << IfEq(label0)
      compile(rhs,ch,cs,map)
      ch <<
        IfEq(label0) <<
        Ldc(1) <<
        Goto(labele) <<
        Label(label0) <<
        Ldc(0) <<
        Label(labele)
    }

    def compileOr(lhs: ExprTree, rhs: ExprTree, ch: CodeHandler, cs: ClassSymbol, map: HashMap[String,Int]): Unit = {
      val label1 = ch.getFreshLabel("label1")
      val labele = ch.getFreshLabel("labele")
      compile(lhs,ch,cs,map)
      ch << IfNe(label1)
      compile(rhs,ch,cs,map)
      ch <<
        IfNe(label1) <<
        Ldc(0) <<
        Goto(labele) <<
        Label(label1) <<
        Ldc(1) <<
        Label(labele)
    }

    def compilePlus(e1: ExprTree, e2: ExprTree, ch: CodeHandler, cs: ClassSymbol, map: HashMap[String,Int]): Unit = {
      if (e1.getType == TInt && e2.getType == TInt) {
        compile(e1,ch,cs,map)
        compile(e2,ch,cs,map)
        ch << IADD
        return
      }
      ch << DefaultNew("java/lang/StringBuilder")
      compile(e1,ch,cs,map)
      ch << InvokeVirtual("java/lang/StringBuilder","append","(" + typeToByteCode(e1.getType) + ")Ljava/lang/StringBuilder;")
      compile(e2,ch,cs,map)
      ch << InvokeVirtual("java/lang/StringBuilder","append","(" + typeToByteCode(e2.getType) + ")Ljava/lang/StringBuilder;")
      ch << InvokeVirtual("java/lang/StringBuilder","toString","()Ljava/lang/String;")
    }

    def compileLessThan(e1: ExprTree, e2: ExprTree, ch: CodeHandler, cs: ClassSymbol, map: HashMap[String,Int]): Unit = {
      val labelLt = ch.getFreshLabel("ltLt")
      val labelAfter = ch.getFreshLabel("ltAfter")
      compile(e1,ch,cs,map)
      compile(e2,ch,cs,map)
      ch <<
        If_ICmpLt(labelLt) <<
        Ldc(0) <<
        Goto(labelAfter) <<
        Label(labelLt) <<
        Ldc(1) <<
        Label(labelAfter)
    }

    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.getName

    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }
    generateMainMethodCode(prog.main, sourceName, outDir)
  }
}
