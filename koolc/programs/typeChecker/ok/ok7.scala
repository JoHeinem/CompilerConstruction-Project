object Main {
  def main(): Unit = {

  }
}

class A extends B {

}

class B {

}

class C {
  def x(): B = {
    return new A();
  }
}
