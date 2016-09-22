object ok9 {
  def main():Unit= {
    println(new Z().x());
  }
}

class Z {
  def x(): String = {
    var a: Int[];
    a = new Int[1];
    a = this.z(a, 1);
    return "ok" + a[0];
  }

  def z(i: Int[], s: Int): Int[] = {
    return new Int[s];
  }
}
