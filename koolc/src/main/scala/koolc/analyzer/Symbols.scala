package koolc
package analyzer

import utils._
import Types._
import koolc.ast.Trees._

object Symbols {
  trait Symbolic[S <: Symbol] {
    private var _sym: Option[S] = None

    def setSymbol(sym: S): this.type = {
      _sym = Some(sym)
      this
    }

    def getSymbol: S = _sym match {
      case Some(s) => s
      case None => sys.error("Accessing undefined symbol.")
    }
  }

  sealed abstract class Symbol extends Positioned with Typed {
    val id: Int = ID.next
    val name: String
    var accessed: Boolean = false
  }

  private object ID {
    private var c: Int = 0

    def next: Int = {
      val ret = c
      c = c + 1
      ret
    }
  }

  class GlobalScope {
    var mainClass: ClassSymbol = _
    var classes = Map[String,ClassSymbol]()

    def lookupClass(n: String): Option[ClassSymbol] = classes.get(n)
  }

  class ClassSymbol(val name: String) extends Symbol {
    var parent: Option[ClassSymbol] = None
    var genericSymbol: Option[GenericSymbol] = None
    var methods = Map[String,MethodSymbol]()
    var members = Map[String,VariableSymbol]()

    def lookupMethod(n: String): Option[MethodSymbol] = methods.get(n)
    def lookupVar(n: String): Option[VariableSymbol] = {
      members.get(n) match {
        case Some(member) => Some(member)
        case None =>
          parent match {
            case Some(parent) => parent.lookupVar(n)
            case None => None
          }
      }
    }
  }

  class MethodSymbol(val name: String, val classSymbol: ClassSymbol) extends Symbol {
    var params = Map[String,VariableSymbol]()
    var members = Map[String,VariableSymbol]()
    var argList: List[VariableSymbol] = Nil
    var overridden : Option[MethodSymbol] = None

    def lookupVar(n: String): Option[VariableSymbol] = {
      if(params.contains(n)){
        Option(params(n))
      }else if(members.contains(n)){
        Option(members(n))
      }else{
        None
      }
    }
  }

  class VariableSymbol(val name: String) extends Symbol

  class GenericSymbol(val name: String) extends Symbol
}
