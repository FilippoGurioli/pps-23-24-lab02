package task4

object Es7:
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
