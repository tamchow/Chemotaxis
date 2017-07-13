package chemotaxis.biology

import javafx.scene.paint.Color

import chemotaxis.biology.FoodSource.Scales
import chemotaxis.extensions.Extensions.Point
import chemotaxis.extensions.MathUtilities._
import chemotaxis.geometry.CircularBiologicalRestrictedShape

/**
  * Represents an entity which can be fed on
  */
trait FoodLike extends CircularBiologicalRestrictedShape {

  import FoodLike._

  val id: Int
  val location: Point
  val amount: Double
  val toxicity: Double
  val maxAmount: Double
  val environment: Environment
  val color: Option[Color]

  def defaultColor: Color

  def toSerializableString: String

  def consumed(hunger: Double): FoodLike

  val _maxAmount: Double = calcMaxAmount(verify(maxAmount, environment.innerRadius))

  private lazy val (randomAmount, randomToxicity) = FoodSource.randomAmountAndToxicity()

  val toxicityRatio: Double = clampNormalized(verify(toxicity, randomToxicity))
  val amountRatio: Double = clampNormalized(verify(amount, randomAmount))
  val _amount: Double = calcAmount(amountRatio, _maxAmount)
  val _toxicity: Double = calcAmount(toxicityRatio, _maxAmount)

  override lazy val visibleColor: Color = color.getOrElse(defaultColor)

  val scales: Scales = Scales()
  val viscosity: Double =
    clampNatural(verify(scales.viscosityScale, FoodSource.Defaults.viscosityScale)) *
    verify(environment.viscosity, Environment.agarViscositySI)
  val nutrientToToxicityRatio: Double = amountRatio / toxicityRatio

  private def controlledHunger(hunger: Double) = clampNormalized(verify(hunger, Bacterium.Defaults.initialHunger))

  def calcAmountAfterConsumption(hunger: Double): Double = amountRatio * coNormalizedLogisticFunction(controlledHunger(hunger))

  def calcToxicityAfterConsumption(hunger: Double): Double = toxicityRatio * (1 + normalizedLogisticFunction(controlledHunger(hunger)))
}

object FoodLike {

  def calcMaxAmount(maxAmount: Double): Double =
    clampNonNegative(maxAmount / 2)

  def calcAmount(amount: Double, maxAmount: Double): Double =
    clampNormalized(amount) * maxAmount
}