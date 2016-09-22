object ok7 {
  def main(): Unit = {
    println(new A().x());
    println(1 < 2);
  }
}

class A {
  def x(): Int = {
    var b: B;
    b = new C().getNewB();
    println(b.dostuff());
    return 0;
  }
}

class B {
  def dostuff(): Int = {
    println("ok");
    return 0;
  }
}

class C {
  def getNewB(): B = {
    var z: B;
    z = new B();
    return z;
  }
}
