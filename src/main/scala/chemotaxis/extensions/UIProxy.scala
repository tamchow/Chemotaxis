package chemotaxis.extensions

import chemotaxis.ui

/**
  * Contains common functions proxied to [[chemotaxis.ui.View]]
  */
trait UIProxy[T] {
  //noinspection TypeAnnotation
  val rng = ui.View.rng
  // The lack of a type annotation is intentional -
  // it allows changing the implementation without having to update the types everywhere.

  def log(message: String): Unit = ui.View.log(message)

  def fromSerializableString(serializedString: String): T = ??? // TODO: Allow deserialization
}
