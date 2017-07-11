package chemotaxis.geometry

import javafx.scene.shape.{Circle, Ellipse, Shape}
import javafx.scene.{paint, canvas => jfxCanvas}

import chemotaxis.extensions.Extensions._

/**
  * Abstract trait containing common functionality for geometry.
  */
trait RestrictedShape {
  type T <: Shape

  def shape: T

  def draw(canvas: jfxCanvas.Canvas): Unit

  def within(point: Point): Boolean = shape.contains(point.toPoint2D)

  def intersects(that: Shape): Boolean =
    shape.getBoundsInParent.intersects(that.getBoundsInParent)

  def overlaps(that: Shape): Boolean =
    shape.getBoundsInParent.contains(that.getBoundsInParent)

  def visiblePaint: paint.Paint = visibleColor

  def visibleColor: paint.Color

  def overlaps
  (that: RestrictedShape): Boolean =
    overlaps(that.shape)

  def intersects
  (that: RestrictedShape): Boolean =
    intersects(that.shape)

  def area: Double = shape match {
    case circle: Circle =>
      Math.PI * circle.getRadius.squared
    case ellipse: Ellipse =>
      Math.PI * ellipse.getRadiusX * ellipse.getRadiusY
    case _ =>
      screenArea
  }

  lazy val screenArea: Double = {
    val bounds = shape.getBoundsInParent
    val (depth, breadth, height) = (bounds.getDepth, bounds.getWidth, bounds.getHeight)
    depth * breadth + breadth * height + height * depth
  }
}
