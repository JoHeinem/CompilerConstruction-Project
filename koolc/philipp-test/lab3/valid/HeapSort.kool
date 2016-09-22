/* 
 * Implements HeapSort alogorithm.
 * Used as reference: 
 * http://www.inf.fh-flensburg.de/lang/algorithmen/sortieren/heap/heapen.htm
*/

object HeapSort {
	def main() : Unit = {
		println(new HS().init().start());
	}
}

class HS {
	var array : Int[]; // Array to sort
	var initialSize : Int; // Size of the array
	var size : Int;	// Size of the part of the array not sorted yet
	var foo : Int; // Used to call method without useful return
	
	def start() : String = {
		
		foo = this.print();
		
		foo = this.createHeap();
		
		foo = this.print();
		
		while(1 < size){
			size = size - 1;
			foo = this.exchange(0, size);
			foo = this.siftDown(0);
			foo = this.print();
		}
		return "Sorting using HeapSort algorithm completed";
	}
	
	def init() : HS = {
		size = 9;
		initialSize = 9;
		array = new Int[size];
		array[0] = 13;
		array[1] = 4;
		array[2] = 27;
		array[3] = 12;
		array[4] = 14;
		array[5] = 18;
		array[6] = 2;
		array[7] = 1;
		array[8] = 3;
		return this;
	}
	
	def createHeap() : Int = {
		var k : Int;

		k = size / 2 - 1;
		
		while(0-1 < k){
			foo = this.siftDown(k);
			k = k - 1;
		}
		
		return 0;
	}
	
	def siftDown(kk: Int) : Int = {
		var k : Int;
		var desc : Int;
		
		k = kk;
		desc = 2 * k + 1; // First descendant of k
		
		while(desc < size){
			if(desc + 1 < size) // Does k has a second descendant ?
				if (array[desc] < array[desc + 1])
					desc = desc + 1; // desc est le plus grand descendant
			
			if(array[desc] - 1 < array[k]) { // k have heap property
				desc = size; // == return
			}else{
				foo = this.exchange(k, desc);
				k = desc;
				desc = 2 * k + 1;
			}
		}
		
		return 0;
	}
	
	def exchange(i: Int, j: Int) : Int = {
		var temp: Int;
		temp = array[i];
		array[i] = array[j];
		array[j] = temp;
		return 0;
	}
	
	def print() : Int = {
		var j : Int;
		var str : String;
		
		j = 0 ;
		str = "";
		
		while (j < initialSize) {
			str = str + array[j];
			j = j + 1 ;
			if(j < initialSize)
				str = str + "-";
		}
		println(str);
		return 0 ;
	}
}
