object Simple {
  def main(): Unit = {
    println("string");
    if (new A().x(2) == 2) {
      println("ok");
    }
  }
}

class A {
  def x(a: Int): Int = {
    var b: Int;
    return a;
  }
}
