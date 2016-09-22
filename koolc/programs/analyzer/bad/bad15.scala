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
  def y(s: Int): String = {
    return c;
  }
}
