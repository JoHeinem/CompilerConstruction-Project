object Simple {
  def main(): Unit = {
    println((new B()).x(2));
  }
}

class A extends B {
  def x(a: Int): Int = {
    var b: Int;
    return b;
  }
}

class B {
  var q: Int;
  def x(a: Int): Int = {
    return this.x(2);
  }
}
