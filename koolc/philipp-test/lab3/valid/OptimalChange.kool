object OptimalChange {
	def main() : Unit = {
		if(new OptChange().calculateChange(3547) == 0) 
			println("Nothing left");
		else 
			println("Error");
	}
}

class OptChange {
	def calculateChange(num : Int) : Int = {
		var coins : Int[];
		var values : Int[];
		
		var current : Int;
		var i : Int;
		
		
		coins = new Int[10];
		values = new Int[coins.length];
		
		coins[0] = 1000;
		coins[1] = 500;
		coins[2] = 200;
		coins[3] = 100;
		coins[4] = 50;
		coins[5] = 20;
		coins[6] = 10;
		coins[7] = 5;
		coins[8] = 2;
		coins[9] = 1;
		
		current = num;
		
		println("Received "+num+".-");
		
		i = 0;
		while(i < coins.length){
			values[i] = current/coins[i];
			current = current - values[i]*coins[i];
			i = i + 1;
		}
		
		i = 0;
		while(i < coins.length){
			if(values[i] == 1){
				println(values[i]+" coin/bill of "+coins[i]+".-");
			}else{
				println(values[i]+" coins/bills of "+coins[i]+".-");
			}
			i = i + 1;
		}
		
		return current;
	}
}
