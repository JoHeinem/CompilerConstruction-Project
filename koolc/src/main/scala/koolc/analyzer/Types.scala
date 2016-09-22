package koolc
package analyzer

import Symbols._
import koolc.ast.Trees._

object Types {
  trait Typed {
    self =>

    private var _tpe: Type = TUntyped

    def setType(tpe: Type): self.type = { _tpe = tpe; this }
    def getType: Type = { _tpe }
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _ => false
    }
    override def toString = "int"
  }

  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case _ => false
    }
    override def toString = "bool"
  }

  case object TIntArray extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TIntArray => true
      case _ => false
    }
    override def toString = "int[]"
  }

  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _ => false
    }
    override def toString = "string"
  }

  case class TGeneric(genericSymbol: GenericSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TGeneric(_) => true
      case _ => false
    }
    override def toString = "generic"
  }

  case class TObject(classSymbol: ClassSymbol, gen: Option[TypeTree]) extends Type {
    // TODO: Extend simple implementation
    private def checkSubType(classSymbol: ClassSymbol, tpe: Type): Boolean = {
      classSymbol.parent match {
        case Some(s) => TObject(s,None).isSubTypeOf(tpe)
        case None => false
      }
    }

    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case obj: TObject => {
        val sameClass: Boolean = this.classSymbol.name == obj.classSymbol.name
        val subClass: Boolean = checkSubType(classSymbol, tpe)
        sameClass || subClass
      }
      case _ => false
    }

    override def toString = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses
  val anyObject = TObject(new ClassSymbol("Object"), None)
}
