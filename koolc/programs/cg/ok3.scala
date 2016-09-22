object ok3 {
  def main(): Unit = {
    println(new Int[3].length);
    if (1 == 1) println("equals works");
    else println("equals does not work");
    println(1 == 2);
    println(1 == 1);
    println(2 == 1);
    println("a" == "a");
  }
}
