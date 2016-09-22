object thisIsAnIdentifier {
  def main(): Unit = {
  }
}

class ThisISAClass [T] {
  var a: Int[];

}

class AnotherClass {
  var b: ThisISAClass[String];

  def x(): String = {
    b = new ThisISAClass[String]();
    return "www";
  }
}
