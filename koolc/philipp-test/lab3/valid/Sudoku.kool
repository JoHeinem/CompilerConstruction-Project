object Sudoku {
    def main() : Unit = {
        println(new SudokuGrid().init().test1());
    }
}

class SudokuGrid {

    var grid_ : Int[];
    var currentLine_ : Int;
    var currentColumn_ : Int;
    var garbage_ : Bool;

    def init() : SudokuGrid = {
        var i : Int;

        grid_ = new Int[81];
        currentLine_ = 0;
        currentColumn_ = 0;
        i = 0;
        while (i < grid_.length) {
            grid_[i] = 0;
            i = i + 1;
        }
        return this;
    }

    def test1() : String = {
        /* fill the sudoku grid */
        garbage_ = this.set(0, 0, 1);
        garbage_ = this.set(0, 5, 7);
        garbage_ = this.set(0, 7, 9);
        garbage_ = this.set(1, 1, 3);
        garbage_ = this.set(1, 4, 2);
        garbage_ = this.set(1, 8, 8);
        garbage_ = this.set(2, 2, 9);
        garbage_ = this.set(2, 3, 6);
        garbage_ = this.set(2, 6, 5);
        garbage_ = this.set(3, 2, 5);
        garbage_ = this.set(3, 3, 3);
        garbage_ = this.set(3, 6, 9);
        garbage_ = this.set(4, 1, 1);
        garbage_ = this.set(4, 4, 8);
        garbage_ = this.set(4, 8, 2);
        garbage_ = this.set(5, 0, 6);
        garbage_ = this.set(5, 5, 4);
        garbage_ = this.set(6, 0, 3);
        garbage_ = this.set(6, 7, 1);
        garbage_ = this.set(7, 1, 4);
        garbage_ = this.set(7, 8, 7);
        garbage_ = this.set(8, 2, 7);
        garbage_ = this.set(8, 6, 3);

        println("~~~~ Original sudoku ~~~~");
        garbage_ = this.display();
        println("Solving sudoku...");  
        if (this.solve()) {
            println("~~~~~ Solved sudoku ~~~~~");
            garbage_ = this.display();
        } else {
            println("Couldn't solve this sudoku :-(");
        }
        return "";
    }

    def set(line : Int, column : Int, value : Int) : Bool = {
        grid_[line * 9 + column] = value;
        return true;
    }
    
    def get(line : Int, column : Int) : Int = {
        return grid_[line * 9 + column];
    }

    /* backtracking algorithm */
    def solve() : Bool = {
        var line : Int;
        var column : Int;
        var value : Int;
        var result : Bool;

        if(!this.findNextUnset()) result = true;
        else result = false;
        line = currentLine_; 
        column = currentColumn_;

        value = 1;
        while (value < 10 && result == false) {
            if (this.isCompatible(line, column, value)) {
                garbage_ = this.set(line, column, value);
                if (this.solve()) {
                    result = true;
                } else {
                    garbage_ = this.set(line, column, 0);
                    currentLine_ = line;
                    currentColumn_ = column;
                }
            }
            value = value + 1;
        }
        return result; 
    }

    def findNextUnset() : Bool = {
        var result : Bool;
        
        result = false;
        while (currentLine_ < 9 && result == false) {
            while (currentColumn_ < 9 && result == false) {
                if (this.isSet(currentLine_, currentColumn_)) currentColumn_ = currentColumn_ + 1;
                else result = true;
            }
            if (result == false) {
                currentLine_ = currentLine_ + 1;
                currentColumn_ = 0;
            }
        }
        return result;
    }

    def isSet(line : Int, column : Int) : Bool = {
        return 0 < this.get(line, column) && this.get(line, column) < 10;
    }

    def isCompatible(line : Int, column : Int, value : Int) : Bool = {
        return !this.appearInLine(line, value) && 
               !this.appearInColumn(column, value) &&
               !this.appearInBlock(line, column, value);
    }

    def appearInLine(line : Int, value : Int) : Bool = {
        var column : Int;
        var result : Bool;

        column = 0;
        result = false;
        while (column < 9 && result == false) {
            if (this.get(line, column) == value) result = true;
            column = column + 1;
        }
        return result;
    }


    def appearInColumn(column : Int, value : Int) : Bool = {
        var line : Int;
        var result : Bool;

        line = 0;
        result = false;
        while (line < 9 && result == false) {
            if (this.get(line, column) == value) result = true;
            line = line + 1;
        }
        return result;
    }

    def appearInBlock(line : Int, column : Int, value : Int) : Bool = {
        var i : Int;
        var j : Int;
        var firstLine : Int;
        var firstColumn : Int;
        var result : Bool;

        firstLine = line / 3 * 3;
        firstColumn = column / 3 * 3;
        i = 0;
        result = false;
        while (i < 3 && result == false) {
            j = 0;
            while (j < 3 && result == false) {
                if (this.get(firstLine + i, firstColumn + j) == value) result = true;
                j = j + 1;
            }
            i = i + 1;
        }
        return result;
    }

    def display() : Bool = {
        var i : Int;
        var separator : String;

        i = 0;
        separator = "+-------+-------+-------+";
        while (i < 9) {
            if (this.mod(i, 3) == 0) println(separator);
            println(this.lineDisplay(i));
            i = i + 1;
        }
        println(separator);
        return true;
    }

    def lineDisplay(line : Int) : String = {
        var column : Int;
        var result : String;

        column = 0;
        result = "";
        while (column < 9) {
           if (this.mod(column, 3) == 0) result = result + "| ";
           if (this.isSet(line, column)) result = result + this.get(line, column) + " ";
           else result = result + "  ";
           column = column + 1;
        }
        return result + "|";
    }

    def mod(i : Int, j : Int) : Int = { return i - (i / j * j); }
}

