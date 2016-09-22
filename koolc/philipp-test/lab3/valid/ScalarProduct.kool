object ScalarProduct {
	def main() : Unit = {
		if(new Sim().start()) {println("Ok");} else {println("error");}        
	}
}

class Sim {
	var v1: Vector4;
	var v2: Vector4;
	var v3: Vector4;
	
	def start(): Bool = {
		v1 = new Vector4();
		v2 = new Vector4();
		v3 = new Vector4();

		println(v1.initialize("v1", 5, 3, 0, 1));
		println(v2.initialize("v2", 0, 3, 4, 7));
		println(v3.initialize("v3", 2, 1, 9, 5));
		
		println("Scalar product " + v1.getName() + "." + v2.getName() + ": " + v1.scalar(v2));
		println("Scalar product " + v1.getName() + "." + v3.getName() + ": " + v1.scalar(v3));
		println("Scalar product " + v2.getName() + "." + v3.getName() + ": " + v2.scalar(v3));

		return true;
	}
}


class Vector4 {
	var vect: Int[];
	var varName: String;

	def initialize(name: String, n1: Int, n2: Int, n3: Int, n4: Int ): String = {
		vect = new Int[4];
		varName = name;
		vect[0] = n1;
		vect[1] = n2;
		vect[2] = n3;
		vect[3] = n4;
		return "New vector " + varName + " created: [" + vect[0] + "," + vect[1] + "," + vect[2] + "," + vect[3] + "]";	
	}

	def scalar(v: Vector4): Int = {
		var x: Int;
		x = vect[0]*v.getInd(0) + vect[1]*v.getInd(1) + vect[2]*v.getInd(2) + vect[3]*v.getInd(3);
		return x;
	}

	def getName(): String = {
		return varName;
	}

	def getInd(i: Int): Int = {
		return vect[i];
	}
}