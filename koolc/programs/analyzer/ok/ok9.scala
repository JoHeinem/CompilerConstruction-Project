object Ok9 {
  def main(): Unit = {
    // this line doesnt compile
    // println(new A[C]().a(new C(), 1));
    println("testing concat " + new A[String]().a("works",0));
    println(new A[String]().a("Testing return t", 0));
    println(new A[String]().a("A",0) == new A[String]().a("A",1));
    println(new A[C]().a(new C(),0) == new A[C]().a(new C(),0));
  }
}

class A[T] {

  var q: T;

  def a(o: T, w: Int): T = {
    return o;
  }
}

class B {
  var b: A[String];
  var c: A[C];
}

class C {
  def q(c: C): String = {
    return "ok";
  }
}
