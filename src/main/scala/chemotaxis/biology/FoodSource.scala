package chemotaxis.biology

import javafx.scene._
import canvas.Canvas
import input.{MouseButton, MouseEvent}
import paint._
import shape.Circle

import chemotaxis._
import extensions._
import Extensions._
import MathUtilities._
import FoodSource.Scales

/**
  * Represents a food source for bacteria
  */
case class FoodSource(id: Int, location: Point,
                      amount: Double, toxicity: Double, maxAmount: Double,
                      environment: Environment,
                      color: Option[Color] = None,
                      override val scales: Scales = Scales())
  extends FoodLike {

  import FoodSource._
  import geometry.CircularBiologicalRestrictedShape._

  if (!environment.within(location))
    throw new IllegalArgumentException(s"$this spawned outside of environment")
  override val center: Point = location

  override def react(event: MouseEvent): Reaction = {
    if (super.react(event)._1) {
      event.getEventType match {
        case click if click == MouseEvent.MOUSE_CLICKED =>
          log(s"Mouse Click on food source ID $id," +
              s" click location: (${event.getSceneX},${event.getSceneY})," +
              s" element location: ($location)," +
              s" element: $this")
          if (event.getButton == MouseButton.SECONDARY) {
            log(s"Secondary click, removing food source id $id")
            (true, Some(environment.copy(foodSources =
                                           environment.foodSources.filterNot(_ == this),
                                         statistics =
                                           environment.statistics.copy(_consumedFoodSources =
                                                                         environment.statistics.consumedFoodSources + 1))))

          } else defaultPositiveReaction
        case _ => defaultNegativeReaction
      }
    } else defaultNegativeReaction
  }

  override def toString: String =
    s"${getClass.getSimpleName}[" +
    s"ID = $id, location = $location," +
    s" color = (provided = $color, current = $visiblePaint)," +
    s" amount = ($amount, ${_amount}, $amountRatio)," +
    s" toxicity = ($toxicity, ${_toxicity}, $toxicityRatio)," +
    s" maxAmount = ($maxAmount, ${_maxAmount})," +
    s" viscosity = (${scales.viscosityScale}, $viscosity),"

  override def equals(obj: scala.Any): Boolean =
    super.equals(obj) || (obj match {
      case other: FoodSource => id == other.id //location == other.location && _maxAmount == other._maxAmount
      case _ => false
    })

  override lazy val hashCode: Int = id //location.## ^ _maxAmount.##

  lazy val withoutEnvironment: FoodSource =
    copy(environment = Environment.createBare(environment.width, environment.height))

  private val colorRatio = toxicityRatio
  override lazy val defaultColor: Color = Color.hsb(colorRatio * 360, 1.0, 1.0)
  private val size = 2 * _amount

  private lazy val backgroundStops =
    Array(new Stop(0, visibleColor.deriveColor(0.0, 1.0, 1.0 - toxicityRatio, 1.0)),
          new Stop(1.0, visibleColor))

  private lazy val radialFill =
    new RadialGradient(1.0, 0.0, 0.5, 0.5, 1.0,
                       true, CycleMethod.NO_CYCLE, backgroundStops: _*)

  private val (x, y) = (location.x - _amount, location.y - _amount)

  override def consumed(hunger: Double): FoodLike =
    copy(amount = calcAmountAfterConsumption(hunger), toxicity = calcToxicityAfterConsumption(hunger))

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

object FoodSource extends UIProxy[FoodSource] {

  case class Scales(viscosityScale: Double = Defaults.viscosityScale,
                    densityScale: Double = Defaults.densityScale)

  val isExistent: FoodLike => Boolean =
    foodSource => (foodSource.amountRatio > ScreenEpsilon) &&
                  (!(foodSource.toxicityRatio in nbdOf1))

  case object Defaults {
    val (viscosityScale, densityScale) = (2.0, 1.5)
    val (safeLimitMin, safeLimitMax) = (0.1, 0.7)
  }

  def safeLimitRatio: Double =
    rng.nextDouble(Defaults.safeLimitMin, Defaults.safeLimitMax)

  def spawnRandomFoodSource(id: Int, location: (Double, Double), environment: Environment): FoodSource =
    spawnRandomFoodSource(id, location, environment, environment.innerRadius)

  def spawnRandomFoodSource(id: Int, location: (Double, Double), environment: Environment, maxAmount: Double): FoodSource =
    spawnRandomFoodSource(id, location, environment, maxAmount, safeLimitRatio)

  def randomAmountAndToxicity(currentSafeLimitRatio: Double = safeLimitRatio): (Double, Double) =
    (currentSafeLimitRatio, currentSafeLimitRatio min (1 - currentSafeLimitRatio))

  def spawnRandomFoodSource(id: Int, location: (Double, Double), environment: Environment, maxAmount: Double, currentSafeLimitRatio: Double): FoodSource = {
    val (amount, toxicity) = randomAmountAndToxicity(currentSafeLimitRatio)
    FoodSource(id, location,
               amount, toxicity, maxAmount,
               environment, Some(ColorUtilities.randomColor))
  }

  def spawnRandomFoodSource(id: Int, environment: Environment, maxAmount: Double, currentSafeLimitRatio: Double = safeLimitRatio): FoodSource =
    spawnRandomFoodSource(id, environment.randomPointInside(FoodLike.calcAmount(currentSafeLimitRatio, maxAmount)), environment, maxAmount, safeLimitRatio)

  def spawnRandomFoodSources(number: Int, environment: Environment): Seq[FoodSource] =
    spawnRandomFoodSources(number, environment.innerRadius, environment)

  def spawnRandomFoodSources(number: Int, maxAmount: Double, environment: Environment): Seq[FoodSource] =
    spawnRandomFoodSources(number, Seq.fill(number)(maxAmount), environment)

  def spawnRandomFoodSources(number: Int, maxAmounts: Seq[Double], environment: Environment): Seq[FoodSource] = {
    if (number < 0)
      throw new IllegalArgumentException(s"Number of food sources to be spawned, $number, cannot be -ve")
    var foodSources = Seq[FoodSource]()
    while (foodSources.length < number) {
      val currentIndex = foodSources.length
      val foodSource = FoodSource.spawnRandomFoodSource(foodSources.length, environment, maxAmounts(currentIndex))
      if (environment.distinctElement(foodSources, foodSource)) foodSources :+= foodSource
    }
    foodSources
  }
}