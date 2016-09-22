object Simple {
  def main(): Unit = {
    println("string");
  }
}

class A extends B {
  def y(a: Int): Int = {
    var d: Int;
    d = this.x(a);
    return d;
  }
}


class B {
  def x(a: Int): Int = {
    var b: Int;
    return b;
  }
}
