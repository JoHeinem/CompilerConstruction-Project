object Prime {
  def main() : Unit = {
    println(new primeTest().init());        
  }
}


class primeTest {

  var n : Int;
  var tab : Int[];

  def mod(m : Int, n : Int) : Int = {
    return m - (n * (m / n));
  }


  def test(c : Int) : Bool =
    {
    var i: Int;
    var a : Int;
    var l: Int;
    var toReturn : Bool;
    toReturn = true;
    if(!(c == 2)) {
    i = 0;
    l = 0;
    a=this.mod(c,2);
    if(0==a) {
      toReturn = false;
    } else {
      i=3;
      l=c/2;
      while(toReturn && i<l)
        {
        a=this.mod(c,i);
        if(a==0)
          {
          toReturn = false;
        }
        i=i+2;
      }
    }}
    return toReturn;
  }

  def print() : Int = {
    println("print() ??");
    return 0;
  }

  def init() : Int = {
    var j : Int;
    n=10;
    tab = new Int[10];
    tab[0]=2;
    tab[1]=23;
    tab[2]=225;
    tab[3]=32;
    tab[4]=5;
    tab[5]=89;
    tab[6]=30;
    tab[7]=2;
    tab[8]=10;
    tab[9]=1733;

    j = 0 ;
    while (j < n) {
      println("Is "+tab[j]+" prime ?");
      println(this.test(tab[j]));
      j = j + 1 ;
    }


    return 0;
  }


}
