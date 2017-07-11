package chemotaxis.geometry

import javafx.scene.shape.{Circle, Rectangle, Shape}

import chemotaxis.extensions.Extensions._

/**
  * Collision utilities
  */
object Collisions {
  def circleContainsShape(circle: Circle, shape: Shape): Boolean = shape match {
    case rectangle: Rectangle =>
      circleContainsRectangle(circle, rectangle)
    case innerCircle: Circle =>
      circleContainsCircle(circle, innerCircle)
    case other =>
      circleContainsRectangle(circle, other.getBoundsInParent)
  }

  def circleContainsRectangle(circle: Circle, rectangle: Rectangle): Boolean = {
    val dx = (circle.getCenterX - rectangle.getX) max
             ((rectangle.getX + rectangle.getWidth) - circle.getCenterX)
    val dy = (circle.getCenterY - rectangle.getY) max
             ((rectangle.getY + rectangle.getHeight) - circle.getCenterY)
    circle.getRadius * circle.getRadius >=
    (dx.squared + dy.squared)
  }

  def circleContainsCircle(containingCircle: Circle, containedCircle: Circle): Boolean = {
    val d = math.hypot(
      containedCircle.getCenterX - containingCircle.getCenterX,
      containedCircle.getCenterY - containingCircle.getCenterY)
    containingCircle.getRadius > (d + containedCircle.getRadius)
  }

  def circleContainsPoint(circle: Circle, point: Vector2D): Boolean =
    ((point.x - circle.getCenterX).squared +
     (point.y - circle.getCenterY).squared) <=
    circle.getRadius.squared
}
