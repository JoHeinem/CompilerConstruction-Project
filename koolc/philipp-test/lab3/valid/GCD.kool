object MainObject {
	def main() : Unit = {
		{
			println(new GreatestCommonDivisor().init(28,7856));
			println(new GreatestCommonDivisor().init(344,873));
			println(new GreatestCommonDivisor().init(456,0));
			println(new GreatestCommonDivisor().init(33,87));
			println(new GreatestCommonDivisor().init(47,6853));
		}
	}
}

class GreatestCommonDivisor {
	var initA : Int;
	var initB : Int;
	var n : Int;
	var t : Int;
	var r : Int;
	
	
	def init(first : Int, second : Int) : Int = {
		initA = first;				
		initB = second;				
		return this.findGcd();
	}
	
	def findGcd() : Int = {
		if(initB < initA){
			n = this.gcd(initA,initB);
		}else {
			n = this.gcd(initB,initA);
		}
		return n;
	}
	
	def gcd(a : Int, b : Int) : Int = {
		while( !(b==0) ){
			t = b;
			b = this.modulo(a,b);
			a = t;
		}
		return a;
	}
	
	/*
	 *	Return the remainder of the division of number by mod
	 */
	def modulo(number : Int, mod : Int) : Int = {
		r = number;
		while( mod < r || mod == r){
			r = r - mod;
		}
		return r;
	}
}

/* Troll Comment */