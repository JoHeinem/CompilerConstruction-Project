object Main {
  def main(): Unit = {
    if (new C().h() == new C().h()) {
      println("ok");
    }
  }
}

class C {
  var x: X;

  def h(): X = {
    return x.bla();
  }
}

class X {

}
