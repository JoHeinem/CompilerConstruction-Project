object A {
  def main(): Unit = {
    println(1 < 0);
    println(0 < 1);
    println(0 < 0);
    println(1);
    println(true);
    println("Hello, world!");
    println(true && false);
    println(false && true);
    println(true && true);
    println(false && false);
    println(true || false);
    println(1+2);
    println(1+"2");
    println("1"+2);
    println("1"+"2");
    if (true) {
      println("OK");
    } else {
      println("If not working");
    }

    if (1 < 0) {
      println("Less than not working");
    } else {
      println("OK");
    }
    while (false) {
      println("while not working");
    }
  }
}
