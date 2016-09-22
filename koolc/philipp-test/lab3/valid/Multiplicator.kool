object Multiplicator {
  def main() : Unit = {
    if(new PaperMulti().showMultiplication(12345,9876)){
      println("Completed successfully!");
    }
    else{
      println("Fail..");
    }
  }
  
}

class PaperMulti{
  def showMultiplication(factor1: Int, factor2: Int): Bool = {
    var i : Int;
    var result : Int;
    var sum : Int;
    var tempResult : Int;
    var maxLength : Int;
    var factorArray1 : Int[];
    var factorArray2 : Int[];
    var aux : Int;
    var translate : Int;
    var initString : String;
    
    result = factor1 * factor2;
    
    factorArray1 = this.separateFactor(factor1);
    factorArray2 = this.separateFactor(factor2);
    
    maxLength = this.getNumberOfDigits(result);
    i = 0;
    translate = 1;
    sum = 0;
    println("");
    println("Multiplication by hand");
    println("**********************");
    
    aux = this.printNumber(maxLength,factor1," ");
    aux = this.printNumber(maxLength,factor2,"*");
    aux = this.printLine(maxLength);
    
    while (i < (factorArray2.length)){
      tempResult = 0;
      tempResult = factorArray2[i]*factor1*translate;
      sum = sum + tempResult;
      
      if (i == 0) initString = " ";
      else initString = "+";
      
      aux = this.printNumber(maxLength,tempResult,initString);
      i = i + 1;
      translate = translate*10;
    }
    aux = this.printLine(maxLength);
    println(" " + sum);
    println("**********************");
    
    return (sum == result);
  }
  def printNumber(theLength: Int, number: Int, initString: String) : Int = {
    var lineToPrint : String;
    var i : Int;
    var numberLength : Int;
    
    i = 0;
    numberLength = this.getNumberOfDigits(number);
    
    lineToPrint = initString;
    
    while (i < (theLength-numberLength)){
      lineToPrint = lineToPrint + " ";
      i = i + 1;
    }
    println(lineToPrint + number);
    
    return 0;
  }
  def printLine(theLength: Int) : Int = {
    var i : Int;
    var lineToPrint : String;
    i = 0;
    lineToPrint = "-";
    
    while (i < theLength){
      lineToPrint = lineToPrint + "-";
      i = i + 1;
    }
    
    println(lineToPrint);
    return 0;
  }
  // number%divider
  def remainder(number: Int, divider: Int ) : Int = {
    var result : Int;
    var realNew : Int;
    
    result = number/divider;
    realNew = result*divider;
    
    if(number < realNew) {
      result = result - 1;
      realNew = result*divider;
    }
    return number - realNew;
  }
  
  def separateFactor(factor: Int) : Int[] = {
    var rest : Int;
    var factorParts : Int[];
    var counter: Int;
    
    counter = 0;
    factorParts = new Int[this.getNumberOfDigits(factor)];
    while ( 9 < factor){
      rest = this.remainder(factor,10);
      factor = (factor - rest)/10;
      factorParts[counter] = rest;
      counter = counter + 1;
    }
    
    factorParts[counter] = factor;
    
    return factorParts;
  }
  
  def getNumberOfDigits(number: Int) : Int = {
    return this.countDigits(number,10,1);
  }
  
  def countDigits(number: Int, value: Int, counter: Int) : Int = {
    var returnValue : Int;
    if (number < value){
      returnValue = counter;
    }
    else{ 
      returnValue = this.countDigits(number,10*value,counter+1);
    }  
    return returnValue;
  }  
}

// Local Variables:
// mode: scala
// coding: utf-8-unix
// sentence-end-double-space: nil
// End:
