object Ok13 {
  def main(): Unit = {
    println(true || (1/0 == 1));
    println(false && (1/0 == 1));
  }
}
