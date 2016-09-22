object A {
  def main(): Unit = {}
}

class B {
  def x(s: String): Int = {
    return 0;
  }

  def y(s: String, i: Int): Int = {
    return this.x(0);
  }

  def z(): Int = {
    var a: Int;
    var b: String;
    a = this.x("a","b");
    b = "ok";
    return this.y(a,b);
  }
}
