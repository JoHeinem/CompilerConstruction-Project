
// This is a simple implementation of '99 bottles of beer' in Tool.
// I guess everyone saw that one coming :)

// The code is much more convoluted that it would be in Scala, partly
// for the lack of match and lists, but also because we have unnecessary
// variables, assignements and ifs, to keep the reference compiler happy.

// To quote @built (twitter.com/built) about his language 'Wheeler':
// "MY GOD IT'S FULL OF STATE!"

object NinetyNineBottles {
    def main() : Unit = {
        if(new DrunkPirate().sing()) { println("Done!"); }
    } 
}

class DrunkPirate {
    def sing() : Bool = {
        var value : Int;
        value = this.verse(99);
        return true;
    }

    def verse(number: Int) : Int  = {
        var value : Int;
        value = 0;

        println(this.bottles(number) + " of beer on the wall, take one down, pass it around, " + this.bottles(number - 1) + " of beer on the wall.");
        if (1 < number) { value = this.verse(number - 1); }
        return value;
    }

    def bottles(number: Int) : String = {
        var value : String;
        if (number == 0) { value = "no more bottles"; } else {
            if (number == 1) { value = "1 bottle"; } else {
                value = number + " bottles";
            }
        }
        return value;
    }
}
