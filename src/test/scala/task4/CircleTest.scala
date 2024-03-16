package task4

import Es7.*
import org.junit.* 
import org.junit.Assert.*
import task4.Es7.Shape.scale

class CircleTest:

    val circle: Shape.Circle = Shape.Circle(5)

    @Test
    def circleRadius() = 
        assertEquals(5, getRadius(circle))

    @Test
    def circleArea() = 
        val radius = getRadius(circle)
        assertEquals(Math.PI * radius * radius, Circle.area(circle))

    @Test
    def circlePerimeter() =
        val radius = getRadius(circle)
        assertEquals(2 * radius * Math.PI, Shape.perimeter(circle))
    
    @Test
    def circleScale() =
        val scaledCircle = Shape.scale(circle, 2)
        assertEquals(Shape.perimeter(circle) * 2, Shape.perimeter(scaledCircle))

    def getRadius(circle: Shape.Circle): Double = circle match { case Shape.Circle(r) => r }

