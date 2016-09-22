object A {
  def main(): Unit = {
    println((new B()).x());
  }
}

class B {

  var b: B;
  def x(): String = {
    return "www";
  }
}
