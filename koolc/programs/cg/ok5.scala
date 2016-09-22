object A {
  def main(): Unit = { println(new B().x(2)); }
}

class B extends C {
  def x(q: Int): Int = {
    var k: Int;
    a = 2;
    return a;
  }
}

class C {
  var a: Int;

  def z(): C = {
    return this;
  }
}
