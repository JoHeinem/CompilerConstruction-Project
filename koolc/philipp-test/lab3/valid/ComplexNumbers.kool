object ComplexNumbers {
  def main () : Unit = {
    println(new ComplexNumber().init(3, 5).multiply(new ComplexNumber().init(4, 6)).inverse().print());
  }
}

class ComplexNumber {
  var re : Int; /* Real part */
  var im : Int; /* Imaginary part */
  
  def init (re_: Int, im_: Int) : ComplexNumber = {
    re = re_;
    im = im_;
    
    return this;
  }
  
  def inverse () : ComplexNumber = {
    return new ComplexNumber().init(0 - re, 0 - im);
  }
  
  def add (b: ComplexNumber) : ComplexNumber = {
    return new ComplexNumber().init(re + b.real(), im + b.imaginary());
  }
  
  def multiply (b: ComplexNumber) : ComplexNumber = {
    return new ComplexNumber().init(re * b.real() - im * b.imaginary(), im * b.real() + re * b.imaginary());
  }
  
  def real () : Int = {
    return re;
  }
  
  def imaginary () : Int = {
    return im;
  }
  
  def print () : String = {
    println(re);
    println(im);
    
    return "";
  }
}