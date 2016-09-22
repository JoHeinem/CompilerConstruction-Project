object Simple {
  def main(): Unit = {
    println("string");
  }
}

class A extends B {
  def y(a: Int): String = {
    var d: String;
    d = new B().x(a);
    return d;
  }
}


class B {
  def x(a: Int): String = {
    var b: Int;
    return "asdf";
  }
}
