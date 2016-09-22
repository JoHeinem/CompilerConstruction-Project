object A {
  def main(): Unit = {
    println(new B().x());
    println(new B().c().a());
  }
}

class B {
  def x(): String = {
    return "ok";
  }

  def c(): C = {
    return new C();
  }
}

class C {
  def a(): String = {
    return "okk";
  }
}
