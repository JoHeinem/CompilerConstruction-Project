object Calendar {
    def main() : Unit = {
        println("~~~ October 2011 ~~~");
        println(new CalendarImpl().init(10, 2011).display());
        println("~~ September 1988 ~~");
        println(new CalendarImpl().init(9, 1988).display());
        println("~~ February 2000 ~~~");
        println(new CalendarImpl().init(2, 2000).display());
    }
}

class CalendarImpl {

    var month_ : Int;
    var year_ : Int;

    def init(month : Int, year : Int) : CalendarImpl = {
        month_ = month;
        year_ = year;
        return this;
    }

    def isLeap(year : Int) : Bool = {
        var result : Bool;

        if (this.mod(year, 100) == 0) result = this.mod(year, 400) == 0;
        else result = this.mod(year, 4) == 0;
        return result;
    }
    

    def daysInMonth(month : Int, year : Int) : Int = {
        var result : Int;

        if (month == 1 || month == 3 || month == 5 || month == 7 ||
            month == 8 || month == 10 || month == 12) result = 31;
        else result = 30;
        if (month == 2) {
            if (this.isLeap(year)) result = 29;
            else result = 28;
        }
        return result;
    }

    /* monday = 0, tuesday = 1, ... */
    def firstJan(year : Int) : Int = {
        var result : Int;
        var pyear : Int;

        pyear = year - 1;
        result = 365 * pyear + pyear / 4 - pyear / 100 + pyear / 400;
        result = this.mod(result, 7);
        return result;
    }

    def firstDay(month : Int, year : Int) : Int = {
        var result : Int;
        var i : Int;

        result = this.firstJan(year);
        i = 1;
        while (i < month) {
            result = result + this.daysInMonth(i, year);
            i = i + 1;
        }
        result = this.mod(result, 7);
        return result;
    }


    def display() : String = {
        var days : Int;
        var firstDay : Int;
        var count : Int;

        days = this.daysInMonth(month_, year_);
        firstDay = this.firstDay(month_, year_);
        
        println("Mo Tu We Th Fr Sa Su");
        println(this.displayWeek(1, 7 - firstDay));
        count = 7 - firstDay;
        while (count < days) {
            if (days < count + 7) println(this.displayWeek(count + 1, days));
            else println(this.displayWeek(count + 1, count + 7));
            count = count + 7;
        }
        return "";
    }

    def displayWeek(first : Int, last : Int) : String = {
        var current : Int;
        var counter : Int;
        var result : String;

        current = first;
        counter = 0;
        result = "";
        while (counter < 7) {
            if (last + counter < 7 || last < current) {
                result = result + "   ";
            } else {
                if (current < 10) result = result + " " + current + " ";
                else result = result + current + " ";
                current = current + 1;
            }
            counter = counter + 1;
        }
        return result;
    }

    def mod(i : Int, j : Int) : Int = { return i - (i / j * j); }
}

