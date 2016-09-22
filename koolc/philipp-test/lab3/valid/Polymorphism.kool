object Polymorphism {
  def main() : Unit = {
    println("Polymorphism and Reference test suite " + new PolymorphismTest().run());
  }
}

class PolymorphismTest {
  def run(): String = {
    var shape : Shape;
    var unit : Int;
    
    shape = new Shape().init("blue");
    unit = this.displayShape(shape);
    
    shape = new Rectangle().initRectangle("red", 2, 4);
    unit = this.displayShape(shape);

    shape = new Square().initSquare("yellow", 2);
    unit = this.displayShape(shape);

    shape = new Circle().init("green");
    unit = this.displayShape(shape);
    
    println("---------------------");
//    unit = shape.drawTwice(); //should give compilation error
    unit = new Circle().initCircle("").drawTwice();
    
    println("---------------------");
    unit = new ReferenceTest().run();

    return "done";
  }
  
  def displayShape(shape : Shape) : Int = {
    var text : String;
    
    text = "I am a " + shape.getColor();
    text = text + " ";
    text = text + shape.getName();
    println(text);
    return shape.draw();
  }
}

class ReferenceTest {
  def run(): Int = {
    var unit : Int;
    
    var bool1 : Bool;
    var bool2 : Bool;
    var int1 : Int;
    var int2 : Int;
    var intArr1 : Int[];
    var intArr2 : Int[];
    var string1 : String;
    var string2 : String;
    var shape1 : Shape;
    var shape2 : Shape;
    
    bool1 = true;
    bool2 = false;
    unit = this.swapBool(bool1, bool2);
    if(bool1) 
      println("bool1: true");
    else
      println("bool1: false");
    if(bool2) 
      println("bool2: true");
    else
      println("bool2: false");
    
    int1 = 1;
    int2 = 2;
    unit = this.swapInt(int1, int2);
    println("int1: " + int1);
    println("int2: " + int2);

    intArr1 = new Int[1];
    intArr2 = new Int[2];
    unit = this.swapIntArr(intArr1, intArr2);
    println("intArr1: " + intArr1.length);
    println("intArr2: " + intArr2.length);
    intArr1[0] = 1;
    intArr2[0] = 2;
    unit = this.swapIntArrValue(intArr1, intArr2);
    println("intArr1val: " + intArr1[0]);
    println("intArr2val: " + intArr2[0]);

    string1 = "1";
    string2 = "2";
    unit = this.swapString(string1, string2);
    println("string1: " + string1);
    println("string2: " + string2);
    unit = this.concatStringValue(string1, string2);
    println("string1val: " + string1);
    println("string2val: " + string2);
    
    shape1 = new Shape().init("1");
    shape2 = new Shape().init("2");
    unit = this.swapShape(shape1, shape2);
    println("shape1: " + shape1.getColor());
    println("shape2: " + shape2.getColor());
    unit = this.resetShape(shape1, shape2);
    println("shape1new: " + shape1.getColor());
    println("shape2set: " + shape2.getColor());

    return 0;
  }

  def swapBool(n1 : Bool, n2 : Bool) : Int = {
    var temp : Bool;
    temp = n1;
    n1 = n2;
    n2 = temp;
    return 0;
  }

  def swapInt(n1 : Int, n2 : Int) : Int = {
    var temp : Int;
    temp = n1;
    n1 = n2;
    n2 = temp;
    return 0;
  }

  def swapIntArr(n1 : Int[], n2 : Int[]) : Int = {
    var temp : Int[];
    temp = n1;
    n1 = n2;
    n2 = temp;
    return 0;
  }

  def swapIntArrValue(n1 : Int[], n2 : Int[]) : Int = {
    var temp : Int;
    temp = n1[0];
    n1[0] = n2[0];
    n2[0] = temp;
    return 0;
  }

  def swapString(n1 : String, n2 : String) : Int = {
    var temp : String;
    temp = n1;
    n1 = n2;
    n2 = temp;
    return 0;
  }

  def concatStringValue(n1 : String, n2 : String) : Int = {
    n1 = n1 + n2;
    return 0;
  }
  
  def swapShape(n1 : Shape, n2 : Shape) : Int = {
    var temp : Shape;
    temp = n1;
    n1 = n2;
    n2 = temp;
    return 0;
  }

  def resetShape(n1 : Shape, n2 : Shape) : Int = {
    var unit : Int;
    n1 = new Shape().init("3");
    unit = n2.setColor("3");
    return 0;
  }
}

class Shape {
  var color : String;
  
  def init(col: String) : Shape = {
    color = col;
    return this;
  }
  
  def getName() : String = {
    return "shape";
  }
  
  def draw() : Int = {
    println("~");
    return 0;
  }
  
  def getColor() : String = {
    return color;
  }
  
  def setColor(col: String) : Int = {
    color = col;
    return 0;
  }
}

class Rectangle extends Shape {
  var x : Int;
  var y : Int;
  
  def initRectangle(c: String, a: Int, b: Int) : Rectangle = {
    var unit: Shape;
    x = a;
    y = b;
    unit = this.init(c);
    return this;
  }
  
  def getName() : String = {
    return "rectangle";
  }
  
  def draw() : Int = {
    var first: String;
    var second: String;
    var i: Int;
    i = 0;    
    first = " ";
    second = "|";
    while(i < x) {
      first = first + "-";
      second = second + " ";
      i = i + 1;
    }
    second = second + "|";

    println(first);
    i = 0;
    while(i < y) {
      println(second);
      i = i + 1;
    }
    println(first);    
    return 0;
  }
}

class Square extends Rectangle {
  def initSquare(c: String, side: Int) : Square = {
    var unit: Shape;
    unit = this.initRectangle(c, side, side);
    return this;
  }

  def getName() : String = {
    return "square";
  }
}

class Circle extends Shape {
  def initCircle(c: String) : Circle = {
    var unit: Shape;
    unit = this.init(c);
    return this;
  }
  
  def getName() : String = {
    return "circle";
  }
  
  def draw() : Int = {
    println("O");    
    return 0;
  }
  
  def drawTwice() : Int = {
    return this.draw() + this.draw();
  }
}

