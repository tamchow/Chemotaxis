package chemotaxis.biology

import javafx.scene._
import canvas.Canvas
import input.MouseEvent
import paint._
import shape.Circle

import chemotaxis._
import extensions._
import Extensions._
import MathUtilities._
import FoodSource._

/**
  * Represents a food source for bacteria
  */
case class FoodSource(location: Point, id: Int,
                      amount: Double, toxicity: Double, maxAmount: Double,
                      environment: Plate,
                      color: Option[Color] = None, scales: Scales = Scales())
  extends geometry.CircularBiologicalRestrictedShape {
  def toSerializableString: String = ??? //TODO: Implement serialization support

  override val center: Point = location

  override def react(event: MouseEvent): Boolean = {
    if (super.react(event)) {
      // TODO: Implement something here if necessary
      if (event.getEventType == MouseEvent.MOUSE_CLICKED) {
        println(s"Mouse Click on food source ID $id," +
                s" click location: (${event.getSceneX},${event.getSceneY})," +
                s" element location: ($location)," +
                s" element: $this")
      }
      true
    } else false
  }

  val _maxAmount: Double = calcMaxAmount(maxAmount)
  val _amount: Double = calcAmount(amount, _maxAmount)
  val amountRatio: Double = clampNormalized(amount)
  val _toxicity: Double = calcAmount(amount, _maxAmount)
  val toxicityRatio: Double = clampNormalized(toxicity)
  val viscosity: Double = clampNatural(scales.viscosityScale) * environment.viscosity
  val density: Double = clampNatural(scales.densityScale) * environment.density
  val kinematicViscosity: Double = viscosity / density

  override def toString: String =
    s"${getClass.getSimpleName}[" +
    s"ID = $id, location = $location," +
    s" color = (provided = $color, current = $visiblePaint)," +
    s" amount = ($amount, ${_amount}, $amountRatio)," +
    s" toxicity = ($toxicity, ${_toxicity}, $toxicityRatio)," +
    s" maxAmount = ($maxAmount, ${_maxAmount})," +
    s" viscosity = (${scales.viscosityScale}, $viscosity)," +
    s" density = (${scales.densityScale}, $density)]"

  override def equals(obj: scala.Any): Boolean =
    super.equals(obj) || (obj match {
      case other: FoodSource => id == other.id //location == other.location && _maxAmount == other._maxAmount
      case _ => false
    })

  override lazy val hashCode: Int = id.## //location.## ^ _maxAmount.##

  lazy val withoutEnvironment: FoodSource =
    copy(environment = Plate.createBarePlate(environment.width, environment.height))

  private val colorRatio = toxicityRatio
  private lazy val defaultColor = Color.hsb(colorRatio * 360, 1.0, 1.0)
  override lazy val visibleColor: Color = color.getOrElse(defaultColor)
  private val size = 2 * _amount
  val nutrientToToxicityRatio: Double = amountRatio / toxicityRatio

  private lazy val backgroundStops =
    Array(new Stop(0, visibleColor.deriveColor(0.0, 1.0, 1.0 - toxicityRatio, 1.0)),
          new Stop(1.0, visibleColor))

  private lazy val radialFill =
    new RadialGradient(1.0, 0.0, 0.5, 0.5, 1.0,
                       true, CycleMethod.NO_CYCLE, backgroundStops: _*)

  private val (x, y) = (location.x - _amount, location.y - _amount)

  def consumed(hunger: Double): FoodSource =
    copy(amount = amountRatio * coNormalizedLogisticFunction(hunger),
         toxicity = toxicityRatio * (1 + normalizedLogisticFunction(hunger)))

  lazy val shape: Circle =
  //@formatter:off
    BuilderStyle.build(new Circle(location.x, location.y, _amount)) { _.setFill(visiblePaint) }
  //@formatter:on

  def draw(canvas: Canvas): Unit = {
    val graphics = canvas.getGraphicsContext2D
    graphics.save()
    graphics.setFill(radialFill)
    graphics.fillOval(x, y, size, size)
    graphics.restore()
  }
}

object FoodSource {

  case class Scales(viscosityScale: Double = Defaults.viscosityScale,
                    densityScale: Double = Defaults.densityScale)

  val isExistent: FoodSource => Boolean =
    foodSource => (foodSource.amountRatio > ScreenEpsilon) &&
                  (!(foodSource.toxicityRatio in nbdOf1))

  private def calcMaxAmount(maxAmount: Double) =
    clampNonNegative(maxAmount / 2)

  private def calcAmount(amount: Double, maxAmount: Double) =
    clampNormalized(amount) * maxAmount

  case object Defaults {
    val (viscosityScale, densityScale) = (2.0, 1.5)
    val (safeLimitMin, safeLimitMax) = (0.1, 0.7)
  }

  private def safeLimitRatio: Double =
    random.nextDouble(Defaults.safeLimitMin, Defaults.safeLimitMax)

  private val random = ui.View.random

  def spawnRandomFoodSource(id: Int, plate: Plate, location: Point, maxAmount: Double): FoodSource =
    spawnRandomFoodSource(id, plate, location, maxAmount, safeLimitRatio)

  def spawnRandomFoodSource(id: Int, plate: Plate, location: Point, maxAmount: Double, currentSafeLimitRatio: Double): FoodSource =
    FoodSource(location, id,
               currentSafeLimitRatio, currentSafeLimitRatio min (1 - currentSafeLimitRatio),
               maxAmount, plate, Some(ColorUtilities.randomColor))

  def spawnRandomFoodSource(id: Int, plate: Plate, maxAmount: Double, currentSafeLimitRatio: Double = safeLimitRatio): FoodSource =
    spawnRandomFoodSource(id, plate,
                          plate.randomPointInside(calcAmount(currentSafeLimitRatio, maxAmount)),
                          maxAmount, safeLimitRatio)

  def spawnRandomFoodSources(number: Int, plate: Plate): Seq[FoodSource] =
    spawnRandomFoodSources(number, plate.innerRadius, plate)

  def spawnRandomFoodSources(number: Int, maxAmount: Double, plate: Plate): Seq[FoodSource] =
    spawnRandomFoodSources(number, Seq.fill(number)(maxAmount), plate)

  def spawnRandomFoodSources(number: Int, maxAmounts: Seq[Double], plate: Plate): Seq[FoodSource] = {
    if (number < 0)
      throw new IllegalArgumentException(s"Number of food sources to be spawned, $number, cannot be -ve")
    var foodSources = Seq[FoodSource]()
    while (foodSources.length < number) {
      val currentIndex = foodSources.length
      val foodSource = FoodSource.spawnRandomFoodSource(foodSources.length, plate, maxAmounts(currentIndex))
      if (plate.distinctElement(foodSources, foodSource)) foodSources :+= foodSource
    }
    foodSources
  }

  def fromSerializableString(serializedString: String): FoodSource = ??? // TODO: Allow deserialization
}
