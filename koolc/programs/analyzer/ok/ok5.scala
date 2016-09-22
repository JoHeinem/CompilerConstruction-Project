object mo {
  def main(): Unit = {
    if (true) {
      println("aaa");
      println("aaa" + 1);
      println(1 + "aaa");
      println("a" + "b");
    } else {
      println(true);
      println(((1 < 2) == (2 < 3)) && (true || false));
      println(2);
      println(1+2);
      println(1*2);
      println(6/2);
    }
  }
}

class Y {
  def x(): Int = {
    var a: Int[];
    a = new Int[3];
    return a[2]*a.length + 3*a[1];
  }

  def q(w: W): W = {
    return w;
  }
}

class W extends Y {

  var h: Y;

  def z(): String = {
    var a: Y;
    //a = new Y();
    return "";//a.x();
  }
}
