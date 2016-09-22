object Simple {
  def main(): Unit = {
    println("string");
  }
}

class A extends B {
  def y(a: Int): String = {
    var str: String;
    var d: String;
    d = "aasdfds";
    str = a + 12;
    str = d + a;
    str = d + "asdfasdf";
    d = new B().x(a);
    return d;
  }
}


class B {
  def x(a: Int): String = {
    var b: Int;
    a = a + 123;
    a = 123 + a;
    a = a + a;
    return "asdf";
  }
}
