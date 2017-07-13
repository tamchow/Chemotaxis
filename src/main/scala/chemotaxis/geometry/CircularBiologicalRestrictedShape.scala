package chemotaxis.geometry

import javafx.scene.input.MouseEvent
import javafx.scene.shape.{Circle, Shape}

import chemotaxis.biology.{FoodSource, Environment}
import chemotaxis.extensions.Extensions._

/**
  * Abstracts out common functionality between [[FoodSource]] and [[Environment]] as both are [[RestrictedShape]]s of [[javafx.scene.shape.Circle]]s
  */
abstract class CircularBiologicalRestrictedShape extends RestrictedShape {
  override type T = Circle

  override def within(point: Point): Boolean =
    Collisions.circleContainsPoint(shape, point)

  override def intersects(that: Shape): Boolean =
    super.intersects(that) || (that match {
      case circle: Circle =>
        Math.hypot(shape.getCenterX - circle.getCenterX,
                   shape.getCenterY - circle.getCenterY) <=
        (shape.getRadius + circle.getRadius)
      case _ => false
    })

  def toSerializableString: String = ??? //TODO: Implement serialization support

  def center: Point

  def react(event: MouseEvent): (Boolean, Option[Environment]) =
    (within(event.getSceneX, event.getSceneY), None)

  override def overlaps(that: Shape): Boolean = Collisions.circleContainsShape(shape, that)
}
