/*
Draw different figures in ASCII using a Drawable class and some maths.
*/

object DrawStuff {
    def main() : Unit = {
    	{
    		println(new ConcentricCircles().render());
    		println(new Mandelbrot().render());
    		println(new Cross().render());
    	}	
    }
}

class Drawable {
	// resolution
	var resX: Int;
	var resY: Int;

	// render the drawing by getting its pixel value one by one
	def render(): String = {
		var depth: Int;
		var screen: String;
		var symb: String;
		var x: Int;
		var y: Int;
		
		screen = "";
		symb = "?";
		x = 0; 	y = 0;
		
		// default resolution
		resX = 66;	resY = 31;
		
		while(y < resY) {
			while(x < resX) {
				depth = this.computePixel(x, y);
				
				if(depth < 1) {symb = " ";}
				if(depth == 1) {symb = ".";}
				if(depth == 2) {symb = "o";}
				if(depth == 3) {symb = "O";}
				if(depth == 4) {symb = "0";}
				if(depth == 5) {symb = "@";}
				if(5 < depth) {symb = "#";}

				screen = screen + symb;
				x = x + 1;
			}
			
			println(screen);
			
			screen = "";
			x = 0;
			y = y+1;
		}
		
		println("");
		println(this.getDesc());
		println("Resolution: " + resX+" by "+resY+" characters.");
		println("");println("");println("");
		return "";
	}
	
	def computePixel(x:Int, y:Int): Int = {
		/* "abstract" method */
		return 0;
	}
	
	def getDesc(): String = {
		/* "abstract" method */
		return "";
	}
}

// Concentric circles, giving a representation of successive integer overflows.
class ConcentricCircles extends Drawable {
	def computePixel(x:Int, y:Int): Int = {
		var x0: Int;
		var y0: Int;
		var iter: Int;
		var maxIter: Int;
		var xTemp: Int;
		
		resX = 65;
		resY = 30;
		
		// image scaling & centering
		x0 = x*2500 - 15000;
		y0 = y*4500 - 15000;
		
		x=0; y=0;
		iter = 0-1; 
		maxIter = 100;
		
		while( (x*x + y*y < 100000000) && iter<maxIter) {
			xTemp = x0 + x;
			y = y0 + y;
			x = xTemp;
			iter = iter + 1;
		}
		
		return iter;
	}
	
	def getDesc(): String = {
		return "Concentric circles, giving a representation of successive integer overflows.";
	}
}

// Representation of a Mandelbrot set (not so pretty due to rounding).
class Mandelbrot extends Drawable {
	def computePixel(x:Int, y:Int): Int = {
		// using pseudocode from Wikipedia Mandelbrot page
		// http://en.wikipedia.org/wiki/Mandelbrot_set
		
		var x0: Int;
		var y0: Int;
		var iter: Int;
		var maxIter: Int;
		var xTemp: Int;
		
		resX = 70;
		resY = 40;
		
		// scaling and centering
		x0 = (x-25)/5 - 1;
		y0 = (y-15)/3;
		
		x=0; y=0;
		iter = 0-2; maxIter = 1000;
		
		while( (x*x + y*y < 50) && iter<maxIter) {
			xTemp = (x*x - y*y + x0);
			y = (2*x*y + y0);
			x = xTemp;
			
			iter = iter + 1;
		}
		
		return iter;
	}
	
	def getDesc(): String = {
		return "Representation of a Mandelbrot set (not so pretty due to rounding).";
	}
}

// Another cool pattern.
class Cross extends Drawable {
	def computePixel(x:Int, y:Int): Int = {
		var x0: Int;
		var y0: Int;
		var iter: Int;
		var maxIter: Int;
		var xTemp: Int;
		var s: Int; //scale
		s= 13;
		
		resX = 50;
		resY = 30;
		
		// scaling and centering
		 x0 = ((x*80*s / resX) - 40*s);
		 y0 = ((y*400*s / resY) - 200*s);

		 x=0; y=0;
		 iter = 0-2; maxIter = 10000;
		 
		 while( (y*y - x*x < 10000) && iter<maxIter) {
			 xTemp = (x*x - y*y + x0)/10;
			 y = (2*x*y*x + y0)/100;
			 x = xTemp;
			 iter = iter + 1;
		 }
		
		return iter;
	}
	
	def getDesc(): String = {
		return "Another cool pattern!";
	}
}
















