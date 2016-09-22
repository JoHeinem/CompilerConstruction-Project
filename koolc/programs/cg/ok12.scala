object Ok12 {
  def main(): Unit = {
    println(new Q().run());
  }
}

class Q {
  def run(): String = {
    var i: Int;
    i=0;
    while (i < 10) {
      println(i);
      i = i+1;
    }
    return "done!";
  }
}
