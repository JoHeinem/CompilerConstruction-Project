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

class C extends B{
  def x(s: String): Int = {
    var b: B;
    return 0;
  }
}

class D extends B{
  def x(s: Int): String = {
    return c;
  }
}
