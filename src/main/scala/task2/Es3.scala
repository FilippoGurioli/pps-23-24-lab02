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

    //Es 6
    def gcd(a: Int, b: Int): Int = 
        val max = Math.max(a, b)
        val min = Math.min(a, b)
        (max, min) match
            case (max, min) if max % min == 0 => min
            case (max, min) => gcd(min, max % min)
    //this function should already be tail-recursive due to the fact that when I do the recursion (line 9) no other operations are specified near to it
    
    println(gcd(12, 8)) //4
    println(gcd(14, 7)) //7

    //Es 7
    enum Shape:
        case Rectangle(width: Double, height: Double)
        case Circle(radius: Double)
        case Square(edge: Double)
    
    object Rectangle:
        def area(rect: Shape.Rectangle) = rect match
            case Shape.Rectangle(w, h) => w * h
    
    object Circle:
        def area(circle: Shape.Circle) = circle match
            case Shape.Circle(r) => Math.PI * r * r

    object Square:
        def area(square: Shape.Square) = square match
            case Shape.Square(e) => e * e

    object Shape:
        def perimeter(shape: Shape) = shape match
            case Shape.Rectangle(w, h) => w * 2 + h * 2
            case Shape.Circle(r) => 2 * r * Math.PI
            case Shape.Square(e) => 4 * e
        
        def scale(shape: Shape, alpha: Double) = shape match
            case Shape.Rectangle(w, h) => Shape.Rectangle(w * alpha, h * alpha)
            case Shape.Circle(r) => Shape.Circle(r * alpha)
            case Shape.Square(e) => Shape.Square(e * alpha)