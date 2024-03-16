package task2

object Task2 extends App:
    //3.a
    //case lambda
    val positive: Int => String = x => x match
        case n if n >= 0 => "positive"
        case _ => "negative"

    //case method syntax
    def positiveMethod(x: Int): String = x match
        case n if n >= 0 => "positive"
        case _ => "negative"

    //3.b
    val neg: (String => Boolean) => (String => Boolean) = predicate => predicate match
        case p => x => !p(x)

    def negMethod(predicate: String => Boolean): String => Boolean = predicate match
        case p => x => !p(x)
    
    val empty: String => Boolean = _ == ""
    val notEmpty = negMethod(empty)
    println(notEmpty("foo")) // true
    println(notEmpty("")) // false
    println(notEmpty("foo") && !notEmpty("")) // true.. a comprehensive test

    //3.c
    def genericNeg[X](predicate: X => Boolean): X => Boolean = predicate match
        case p => x => !p(x)
    
    val empty1: String => Boolean = _ == ""
    val notEmpty1 = genericNeg[String](empty)
    println(notEmpty1("foo")) // true
    println(notEmpty1("")) // false
    println(notEmpty1("foo") && !notEmpty1("")) // true.. a comprehensive test

    //4
    val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z
    val p2: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z
    def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z
    def p4(x: Int, y: Int, z: Int): Boolean = x <= y && y == z

    //5
    def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))

    println(compose(_ - 1, _ * 2)(5)) // 9

    def genericCompose[X, Y, Z](f: X => Y, g: Z => X): Z => Y = x => f(g(x))
    
    println(genericCompose[Int, String, Boolean](positive, b =>  b match { case v if v => 1; case _ => -1 })(false)) //negative
    //The only constraint is that the codomain of g must be the domain of f