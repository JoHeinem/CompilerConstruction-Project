object A {
  def main(): Unit = {
  }
}

class B {
  var c: Int;

  def x(s: Int): Int = {
    return c;
  }
}

class D extends B{
  var c: String;

  def y(s: Int): String = {
    return c;
  }
}
