package task4

import Es7.*
import org.junit.* 
import org.junit.Assert.*

class RectTest {

    val rect: Shape.Rectangle = Shape.Rectangle(5, 7)

    @Test
    def rectangleWidth() = 
        assertEquals(5, rect match { case Shape.Rectangle(w, h) => w })

    @Test
    def rectangleHeight() = 
        assertEquals(7, rect match { case Shape.Rectangle(w, h) => h })

    @Test
    def rectangleArea() = 
        assertEquals(7 * 5, Rectangle.area(rect))

    @Test
    def rectanglePerimeter() =
        val (w, h) = getDimensions(rect)
        assertEquals(w * 2 + h * 2, Shape.perimeter(rect))

    @Test
    def rectangleScale() = 
        assertEquals(Shape.perimeter(rect) * 2, Shape.perimeter(Shape.scale(rect, 2)))

    def getDimensions(rect: Shape.Rectangle): Tuple2[Double, Double] = rect match
        case Shape.Rectangle(w, h) => (w, h)
    
}
