object Simple {
  def main(): Unit = {
    println("string");
  }
}

class A extends B {
  def y(a: Int): Int = {
    var d: Int;
    d = this.x(a) + b;
    return d;
  }

  def z(b: Int): Int = {
    return b;
  }
}


class B {
  var b: Int;

  def x(a: Int): Int = {
    return b;
  }
}
