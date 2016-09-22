object A {
  def main(): Unit = {
    if (new B().h() == new C().h()) {
      println("ok");
    }
  }
}

class B extends C {
  def h(): X = {
    return x;
  }
}

class C {
  var x: X;

  def h(): X = {
    return x;
  }
}

class X {

}
