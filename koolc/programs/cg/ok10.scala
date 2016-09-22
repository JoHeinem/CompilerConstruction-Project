object Ok10 {
  def main(): Unit = {
    println(new Q().x());
  }
}

class Q {
  def x(): Int = {
    var unit: Int;
    var intArr1 : Int[];
    var intArr2 : Int[];
    intArr1 = new Int[1];
    intArr2 = new Int[2];
    intArr1[0] = 1;
    intArr2[0] = 2;
    unit = this.swapIntArrValue(intArr1, intArr2);
    return 0;
  }

  def swapIntArrValue(n1 : Int[], n2 : Int[]) : Int = {
    var temp : Int;
    temp = n1[0];
    n1[0] = n2[0];
    n2[0] = temp;
    return 0;
  }

}
