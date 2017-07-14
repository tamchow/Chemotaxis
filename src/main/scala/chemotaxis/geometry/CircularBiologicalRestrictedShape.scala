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

  import CircularBiologicalRestrictedShape._

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

  val environment: Environment

  def react(event: MouseEvent): Reaction =
    if (within(event.getSceneX, event.getSceneY)) defaultPositiveReaction else defaultNegativeReaction

  val defaultPositiveReaction: Reaction = Some(environment)

  override def overlaps(that: Shape): Boolean = Collisions.circleContainsShape(shape, that)
}

object CircularBiologicalRestrictedShape {

  type Reaction = Option[Environment]

  val defaultNegativeReaction: Reaction =
    None
}