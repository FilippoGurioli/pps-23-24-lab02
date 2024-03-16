package task4

import Es7.*
import org.junit.* 
import org.junit.Assert.*

class SquareTest {
  
    val square: Shape.Square = Shape.Square(4)

    @Test
    def squareEdge() =
        assertEquals(4, getEdge(square))

    @Test
    def squareArea() =
        val edge = getEdge(square)
        assertEquals(edge * edge, Square.area(square))

    @Test
    def squarePerimeter() =
        val edge = getEdge(square)
        assertEquals(4 * edge, Shape.perimeter(square))
    
    @Test
    def squareScale() =
        assertEquals(Shape.perimeter(square) * 2, Shape.perimeter(Shape.scale(square, 2)))

    def getEdge(square: Shape.Square) = square match
        case Shape.Square(e) => e
    
}
