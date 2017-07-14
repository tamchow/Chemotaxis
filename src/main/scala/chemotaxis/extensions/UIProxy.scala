package chemotaxis.extensions

import chemotaxis.ui

/**
  * Contains common functions proxied to [[chemotaxis.ui.View]]
  */
trait UIProxy[T] {
  //noinspection TypeAnnotation
  val rng = ui.View.rng
  // The lack of a type annotation above is intentional -
  // it allows changing the implementation without having to update the types everywhere.

  val loggingEnabled: Boolean = ui.View.loggingEnabled

  def log(item: Any): Unit = log(item.toString)

  def log(message: String): Unit =
    if (loggingEnabled) ui.View.log(message) else ()

  def fromSerializableString(serializedString: String): T = ??? // TODO: Allow deserialization
}
