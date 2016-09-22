object Main {
  def main(): Unit = {
    if (new B().h() == new C().h()) {
      println("ok");
    }
  }
}

class A extends B {

  def h(x: String): String = {
    return x;
  }
}


class B extends C {

}

class C {
  var x: X;

  def h(): X = {
    return x;
  }
}

class X {

}
