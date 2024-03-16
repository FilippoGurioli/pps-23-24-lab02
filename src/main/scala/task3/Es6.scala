package task3

object Es6 extends App:
    def gcd(a: Int, b: Int): Int = 
        val max = Math.max(a, b)
        val min = Math.min(a, b)
        (max, min) match
            case (max, min) if max % min == 0 => min
            case (max, min) => gcd(min, max % min)
    //this function should already be tail-recursive due to the fact that when I do the recursion (line 9) no other operations are specified near to it
    
    println(gcd(12, 8)) //4
    println(gcd(14, 7)) //7
        