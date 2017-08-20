package chemotaxis.biology

import javafx.geometry.Dimension2D
import javafx.scene._
import input.{MouseButton, MouseEvent}
import canvas.{Canvas, GraphicsContext}
import paint.Color
import shape.{Circle, Ellipse}
import transform.Rotate

import chemotaxis._
import chemotaxis.extensions.UIProxy
import extensions.Extensions._
import extensions.MathUtilities._
import Bacterium.{BacteriumParameters, RotationParameters}

import scala.util.{Success, Try}

/**
  * Represents one bacterium
  */
case class Bacterium(id: Int, location: Point,
                     size: Dimension2D, angle: Double,
                     initialSpeed: Double,
                     hunger: Double,
                     standardParameters: BacteriumParameters,
                     environment: Environment,
                     parents: List[Bacterium] = Nil,
                     color: Option[Color] = None,
                     rotationParameters: RotationParameters =
                     RotationParameters())
  extends FoodLike {

  import Bacterium._
  import geometry.CircularBiologicalRestrictedShape._

  override def react(event: MouseEvent): Reaction =
    super.react(event) match {
      case Some(_) => event.getEventType match {
        case click if click == MouseEvent.MOUSE_CLICKED =>
          log(s"Mouse Click on bacterium ID $id, click location: (${event.getSceneX}},${event.getSceneY}}), element location: ($location), element: $this")
          if (event.getButton == MouseButton.SECONDARY) {
            log(s"Secondary click, removing bacterium id $id")
            Some(environment.copy(bacteria =
                                    withoutThisBacterium(environment.bacteria),
                                  statistics =
                                    environment.statistics.copy(_deadBacteria =
                                                                  environment.statistics.deadBacteria + 1)))

          } else defaultPositiveReaction
        case _ => defaultNegativeReaction
      }
      case None => defaultNegativeReaction
    }

  private def withoutThisBacterium(bacteria: Seq[Bacterium]) = bacteria.filterNot(_ == this)

  if (!environment.within(location))
    throw new IllegalArgumentException(s"$this spawned outside of environment")

  override val center: Point = location
  private lazy val areaScale =
    clampNonNegative(verify(standardParameters.areaScale, Defaults.scales.areaScale))
  private lazy val fissionLimitingHunger =
    clampNormalized(verify(standardParameters.fissionLimitingHunger, Defaults.fissionLimitingHunger))
  private lazy val hybridizationLimitingHunger =
    clampNormalized(verify(standardParameters.hybridizationLimitingHunger, Defaults.hybridizationLimitingHunger))
  private lazy val unTumbleThreshold =
    clampNormalized(verify(standardParameters.unTumbleThreshold, Defaults.unTumbleThreshold))
  private lazy val _hunger =
    clampNormalized(if (hunger.isNaN) 0.0 else hunger)
  private lazy val hungerIncrement =
    clamp(0, 1 - _hunger)(verify(standardParameters.hungerIncrement, randomHungerIncrement(Defaults.scales, randomSpeed)))

  private def normalizeAngle(angle: Double): Double = {
    val normalizedAngle = angle % PiTimes2
    (if (normalizedAngle < 0.0) PiTimes2 else 0.0) + normalizedAngle
  }

  private lazy val _angle = normalizeAngle(verify(angle, normalizeAngle(verify(rotationParameters.previousAngle, randomAngle))))
  private lazy val previousAngle = verify(normalizeAngle(rotationParameters.previousAngle), _angle)
  lazy val meanRadius: Double = clampNormalized(verify(standardParameters.meanRadius, randomResponseCoefficient))
  lazy val deviation: Double = clampNormalized(verify(standardParameters.deviation, randomResponseCoefficient))
  val maxDistance: Double = environment.diameter
  private lazy val flagella = clampNatural(standardParameters.numberOfFlagella)

  // Just a way to get a color dependent on a unique constant property of a bacterium
  override lazy val defaultColor: Color = Color.hsb(meanRadius * 360, 1.0, 1.0)

  private lazy val scaledFlagellaCount =
    Try(flagella / environment.bacteria.maxBy(_.flagella).flagella.toDouble).getOrElse(1.0)
  // For collision velocity calculations, and other things...
  // These 2 properties below can be expected to be proportional to the number of flagella.
  lazy val mass: Double =
  (1 + scaledFlagellaCount) * clampNatural(verify(standardParameters.massScale, Defaults.scales.massScale)) * (1 - _hunger)
  lazy val force: Double =
    (1 + scaledFlagellaCount) * clampNatural(verify(standardParameters.forceScale, Defaults.scales.forceScale)) * (1 - _hunger) - drag
  private lazy val internalArea = areaScale * mass
  private lazy val lengthScale = math.sqrt(internalArea) * 0.5
  private lazy val (width, height) = (clampNonNegative(size.getWidth), clampNatural(size.getHeight))
  lazy val (radiusX, radiusY) = (lengthScale * (width, height)).roundedDown
  private lazy val diagonal = math.hypot(width, height).round
  private lazy val flagellumLength = (lengthScale * diagonal).round
  private lazy val maxLinearDimension = 2 * flagellumLength
  private lazy val coefficientOfRestitution =
    Try((maxLinearDimension / environment.bacteria.maxBy(_.maxLinearDimension).maxLinearDimension) * scaledFlagellaCount).getOrElse(1.0)
  lazy val speed: Double =
    clampNonNegative(verify(initialSpeed, randomSpeed))
  lazy val scaledSpeed: Double =
    speed / Environment.pixelsPerMeter

  override lazy val amount: Double = 1 - _hunger
  override lazy val toxicity: Double = _hunger
  override lazy val maxAmount: Double = maxLinearDimension
  private val (x, y) = location.roundedDown

  // True Gaussian distribution - appears inefficient,  although it works
  /*
    private lazy val tumbleProbabilityFunction: Double => Double =
      x => gaussianProbability(meanRadius, deviation)(x).min(1 - Epsilon)
  */

  // Simplified Gaussian distribution with dynamic incentivized scaling - appears to be quite efficient
  private val responseCoefficient = meanRadius / deviation

  private val probabilityScaleFactor = math.E * responseCoefficient

  private lazy val tumbleProbabilityFunction: Double => Double =
    x => (probabilityScaleFactor * x.squared * math.exp(-responseCoefficient * x.squared)).min(1 - Epsilon)

  lazy val stopped: Boolean = speed < ScreenEpsilon

  override lazy val toString: String =
    s"${getClass.getSimpleName}[" +
    s"ID = $id, location = ($x, $y)," +
    s" size = $size" +
    //s" parents = $parents" +
    s" color = (provided = $color, current = $visiblePaint)," +
    s" flagella = $flagella (length = $flagellumLength)," +
    s" speed = $speed, response = (mean radius = $meanRadius, deviation = $deviation)," +
    s" un-tumble threshold = $unTumbleThreshold, angle = ${_angle}," +
    s" mass = (${standardParameters.massScale}, $mass)" +
    s" area = (${standardParameters.areaScale}, X radius = $radiusX, Y radius = $radiusY)" +
    s" hunger = (base = ${_hunger}, increment = $hungerIncrement)," +
    s" limiting hunger =" +
    s" (fission = $fissionLimitingHunger," +
    s" hybridization = $hybridizationLimitingHunger)," +
    s" force = (${standardParameters.forceScale}, $force)," +
    s" food source scoring function = ${standardParameters.scoringFunction}]"

  override def equals(obj: scala.Any): Boolean =
    super.equals(obj) || (obj match {
      case other: Bacterium => id == other.id
      case _ => false
    })

  override lazy val hashCode: Int = id

  override def consumed(hunger: Double): FoodLike =
    copy(hunger = calcToxicityAfterConsumption(hunger))

  lazy val generation: Int = parents match {
    case Nil => 1
    case existingParents => 1 + existingParents.length
  }

  private def fed(localEnvironment: Environment, bacterium: Bacterium): (Boolean, Environment, Option[Bacterium]) = {
    val allFoodSources: Seq[FoodLike] = if (standardParameters.cannibal_?)
                                          localEnvironment.foodSources ++ withoutThisBacterium(localEnvironment.bacteria)
                                        else localEnvironment.foodSources
    val feeding = allFoodSources.filter(FoodSource.isExistent)
      // We need the bacteria to be on the food before it can consume it -
      // bacteria feeding by surface absorption is not a baseless assumption.
      // Flagella do not contribute.
      .findWithIndex { // Side Effect:
                       // The smaller a bacterium becomes (due to size-hunger scaling), the smaller food sources it can consume
                       // i.e., the more hungry a bacterium, the more desperate it is to extract nutrition even from low-quality sources
                       foodSource =>
                         (foodSource overlaps bacterium.baseShape) ||
                         bacterium.baseShape.getBoundsInParent.contains(foodSource.shape.getBoundsInParent)
                     }
    // if the bacterium has reached the food, and it is hungry
    val (didFeed, reduceHunger, newFoodSources, newBacteria) =
      if (feeding.isDefined && bacterium._hunger.abs >= Epsilon) {
        // it will consume the food
        val (foodSource, index) = feeding.get
        log(s"$bacterium is feeding on $foodSource")
        val reducedFoodSource = foodSource.consumed(bacterium._hunger)
        // Linear relationship here between hunger, food consumed and toxicity
        val reduceHunger = (1 - (reducedFoodSource._amount / foodSource._amount)) *
                           (1 + foodSource.toxicityRatio) * bacterium._hunger
        val newFoodSources = reducedFoodSource match {
          case realFoodSource: FoodSource => localEnvironment.foodSources.updated(index, realFoodSource)
          case _ => localEnvironment.foodSources
        }
        val newBacteria = reducedFoodSource match {
          case fakeFoodSource: Bacterium =>
            localEnvironment.bacteria.updated(index - localEnvironment.foodSources.length, fakeFoodSource)
          case _ => localEnvironment.bacteria
        }
        (true, reduceHunger, newFoodSources, newBacteria)
      } else (false, 0.0, localEnvironment.foodSources, localEnvironment.bacteria)
    val newHunger = bacterium._hunger - reduceHunger
    val existingFoodSources = newFoodSources.filter(FoodSource.isExistent)
    val currentStatistics = localEnvironment.statistics
    val newStatistics = currentStatistics.copy(_consumedFoodSources =
                                                 currentStatistics.consumedFoodSources +
                                                 (newFoodSources.length - existingFoodSources.length))
    val newEnvironment = localEnvironment.copy(bacteria = newBacteria,
                                               foodSources = existingFoodSources,
                                               statistics = newStatistics)
    if (newHunger < 1 - Epsilon)
      (didFeed, newEnvironment, Some(bacterium.copy(hunger = newHunger)))
    else
      (true, newEnvironment, None)
  }

  private val calcNormalizedDistanceTo: (FoodLike) => Double =
    foodSource => location.distance(foodSource.location) / maxDistance

  private def hybridize(bacterium1: Bacterium, bacterium2: Bacterium):
  Option[(Bacterium, Bacterium)] = {
    // If the hybridizing bacteria belong to the same lineage,
    // hybridization is useless as no genetic variety will be produced.
    if (bacterium1.parents.union(bacterium2.parents).isEmpty) {
      lazy val willHybridize =
        nextBooleanWithMaxProbability(standardParameters.limitingProbabilities.hybridizationLimitingProbability)
      if (bacterium1._hunger < bacterium1.hybridizationLimitingHunger &&
          bacterium2._hunger < bacterium2.hybridizationLimitingHunger &&
          willHybridize) {
        log(s"$bacterium1 hybridizing with $bacterium2")
        val averageResponseCoefficient =
          (bacterium1.meanRadius + bacterium2.meanRadius) / 2.0
        val averageScoringFunction =
          bacterium1.standardParameters.scoringFunction.average(bacterium2.standardParameters.scoringFunction)
        val averageNumberOfFlagella =
          (bacterium1.flagella + bacterium2.flagella) / 2
        val ids = environment.newBacterialIDs(2)
        Some((bacterium1.copy(id = ids.head,
                              parents = bacterium1.parents :+ bacterium1,
                              //color = Some(bacterium1.visibleColor),
                              standardParameters =
                                bacterium1.standardParameters
                                  .copy(meanRadius = averageResponseCoefficient,
                                        scoringFunction = averageScoringFunction,
                                        numberOfFlagella = averageNumberOfFlagella)),
               bacterium2.copy(id = ids.last,
                               parents = bacterium2.parents :+ bacterium2,
                               //color = Some(bacterium2.visibleColor),
                               standardParameters =
                                 bacterium2.standardParameters
                                   .copy(meanRadius = averageResponseCoefficient,
                                         scoringFunction = averageScoringFunction,
                                         numberOfFlagella = averageNumberOfFlagella))))
      } else None
    } else None
  }

  private def divide(bacterium: Bacterium):
  Option[(Bacterium, Bacterium)] = {
    lazy val willDivide =
      nextBooleanWithMaxProbability(standardParameters.limitingProbabilities.fissionLimitingProbability)
    if (_hunger < fissionLimitingHunger && willDivide) {
      log(s"$bacterium is dividing")
      val offsetAngle =
      // ϵ (0.0, 1.0), so exclude the endpoints to a reasonable degree
        rng.nextDouble(Defaults.minUnStickValue, Defaults.maxUnStickValue) * PiBy2
      val newHunger = 0.5 * (1 + _hunger)
      val ids = environment.newBacterialIDs(2)
      val locationOffset =
        ((if (x > environment.center.x) -1 else 1) * size.getWidth,
          (if (y > environment.center.y) -1 else 1) * size.getHeight)
      val childLocations = IndexedSeq(location + locationOffset, location - locationOffset)
      if (childLocations.count(environment.within) == childLocations.length)
        Some((copy(id = ids.head,
                   parents = parents :+ bacterium,
                   location = childLocations(0),
                   angle = _angle + offsetAngle,
                   initialSpeed = speed * math.sin(offsetAngle),
                   hunger = newHunger),
               copy(id = ids.last,
                    parents = parents :+ bacterium,
                    location = childLocations(1),
                    angle = _angle - offsetAngle,
                    initialSpeed = speed * math.cos(offsetAngle),
                    hunger = newHunger)))
      else None
    } else None
  }

  lazy val withoutEnvironment: Bacterium =
    copy(environment = Environment.createBare(environment.width, environment.height))

  private def quorum(foodSource: Try[FoodLike]): Int =
    environment.bacteria.count(bacterium => bacterium != this && bacterium.closestFoodSource == foodSource)

  private def drag: Double = {
    val onFood = environment.foodSources.find(_.overlaps(this.baseShape))
    // Assumptions:
    // 1. Bacteria with flagella of equal length are modelled as a sphere of radius equal to the length of the flagella
    // 2. Flow is laminar as bacteria are streamlined and their movement is slow. (This may be inaccurate, but suffices for our case)
    6 * Pi * onFood.map(_.viscosity).getOrElse(environment.viscosity) * maxLinearDimension * scaledSpeed
  }

  private def calcSpeed(time: Double) =
    speed + ((force / mass) * time)

  /**
    * @note For the score, higher is better
    * @param foodSource the food source to score
    * @return the score of this food source
    */
  private def scoreSource(foodSource: FoodLike) =
    standardParameters.scoringFunction.score(// We want to maximize closeness, not distance
                                             1 - calcNormalizedDistanceTo(foodSource),
                                             foodSource.nutrientToToxicityRatio,
                                             quorum(Success(foodSource)))

  def closestFoodSource: Try[FoodLike] =
    Try(environment.foodSources.minBy(calcNormalizedDistanceTo))

  private def newRotationParameters(current: Bacterium): RotationParameters = {
    import current.rotationParameters._
    current.rotationParameters
      .copy(rotated_? = false,
            previousAngle = _angle,
            _offsetAngle = offsetAngle + offsetAngleIncrement)
  }

  private def distanceToBestCandidate(candidates: Seq[FoodLike]): Try[Double] =
    Try(candidates.maxBy(scoreSource)).map(calcNormalizedDistanceTo)

  def move(time: Double): (Environment, Seq[Option[Bacterium]]) = {
    val (bacterium, localEnvironment) = handleCollisions(environment)
    fed(localEnvironment, bacterium) match {
      case (didFeed, newEnvironment, newBacterium) if didFeed =>
        (newEnvironment, IndexedSeq(newBacterium.map(raw => {
          raw.copy(initialSpeed = speed,
                   environment = newEnvironment,
                   rotationParameters = newRotationParameters(raw))
        })))
      case (_, thisEnvironment, Some(thisBacterium)) =>
        val unaltered = (thisEnvironment, IndexedSeq(Some(thisBacterium)))
        // Handle binary fission
        divide(thisBacterium) match {
          case Some((offspring1, offspring2)) =>
            (thisEnvironment,
              IndexedSeq(Some(offspring1),
                         Some(offspring2),
                         None))
          case None =>
            val r =
              if (standardParameters.cannibal_?)
                distanceToBestCandidate(thisEnvironment.foodSources)
                  .getOrElse(distanceToBestCandidate(withoutThisBacterium(thisEnvironment.bacteria))
                               .getOrElse(1.0))
              else
                distanceToBestCandidate(thisEnvironment.foodSources).getOrElse(1.0)
            val rawTumbleProbability = tumbleProbabilityFunction(r)
            // Ensure that the bacterium doesn't get stuck away from food
            val tumbleProbability = rawTumbleProbability *
                                    (if (math.abs(rawTumbleProbability - unTumbleThreshold) > Epsilon)
                                     // Sensible scaling
                                       rng.nextDouble(Defaults.minUnStickValue, Defaults.maxUnStickValue)
                                     else 1.0)
            val willTumble = rng.nextBoolean(tumbleProbability)
            // angle to tumble through
            val tumbleDirection = if (willTumble) rng.nextDouble(PiTimes2) else 0.0
            val distanceToMove = if (willTumble) 0.0 else speed * time
            // We consider the distance travelled and the current hunger to calculate the new hunger,
            // as these indicate energy reserves and expenditure by the bacterium.
            val newHunger = thisBacterium._hunger + thisBacterium.hungerIncrement +
                            ((distanceToMove / thisBacterium.maxDistance) * thisBacterium._hunger * thisBacterium.hungerIncrement)
            val newAngle = thisBacterium._angle + tumbleDirection
            val newSpeed = calcSpeed(time)
            val newLocation = (location + Vector2D.fromMagnitudeAndAngle(distanceToMove, newAngle)).rounded
            val moveLine = RichLine(location, newLocation)
            val pointOfIntersectionWithBoundary = thisEnvironment.lineIntersectsBoundary(moveLine)
            val nextRotationParameters = newRotationParameters(thisBacterium).copy(rotated_? = willTumble)
            if (pointOfIntersectionWithBoundary.isDefined) {
              val reflectedLocation = pointOfIntersectionWithBoundary.get.rounded
              val currentVelocity = Vector2D.fromMagnitudeAndAngle(newSpeed, newAngle)
              val surfaceNormal = reflectedLocation - thisEnvironment.center
              val currentVelocityProjectedOnSurfaceNormal = surfaceNormal.projectionOf(currentVelocity)
              val reflectedVelocity = currentVelocity - (2 * currentVelocityProjectedOnSurfaceNormal)
              val (reflectedAngle, reflectedSpeed) = (reflectedVelocity.angle, reflectedVelocity.norm)
              // Safeguards
              if (thisEnvironment.within(reflectedLocation))
                (thisEnvironment, IndexedSeq(Some(thisBacterium.copy(location = reflectedLocation,
                                                                     angle = verify(reflectedAngle, _angle),
                                                                     initialSpeed = verify(reflectedSpeed, speed),
                                                                     hunger = newHunger,
                                                                     environment = thisEnvironment,
                                                                     rotationParameters = nextRotationParameters))))
              else unaltered
            }
            else {
              // Safeguards
              if (thisEnvironment.within(newLocation))
                (thisEnvironment, IndexedSeq(Some(thisBacterium.copy(location = newLocation,
                                                                     angle = verify(newAngle, _angle),
                                                                     initialSpeed = verify(newSpeed, speed),
                                                                     hunger = newHunger,
                                                                     environment = thisEnvironment,
                                                                     rotationParameters = nextRotationParameters))))
              else unaltered
            }
        }
    }
  }

  private lazy val flagellarEndCoordinates: Seq[Point] = {
    import rotationParameters._
    val limitingAngle = if (rotated_?) PiTimes2 else Pi + 2 * offsetAngle
    val sectorSweep = limitingAngle / (flagella + (if (rotated_?) 0 else 1))
    //noinspection ScalaUnnecessaryParentheses
    val startingAngle = _angle + PiBy2 + (if (rotated_?) 0.0 else sectorSweep) +
                        (if (rotated_?) (_angle - this.previousAngle) else -offsetAngle)
    //@formatter:off
    IndexedSeq.tabulate(flagella) { flagellumIndex => {
      val offset = flagellumIndex * sectorSweep
      val flagellumAngle = (startingAngle + offset) % PiTimes2
      val relativeAngle = flagellumAngle - _angle
      val surfaceOffset = (radiusX * math.cos(relativeAngle), radiusY * math.sin(relativeAngle)).norm
      location + Vector2D.fromMagnitudeAndAngle(flagellumLength + surfaceOffset, flagellumAngle)
    }}
      //@formatter:on
    // Below mechanic disabled because it looks weird, though it works.
    // .map(newLocation => if (environment.withinPlate(newLocation)) newLocation else (2.0 * location) - newLocation)
  }

  lazy val locationInfo: (Point, Color) = (location, visibleColor)

  def handleCollisions(localEnvironment: Environment): (Bacterium, Environment) = {
    var thisBacterium = copy()
    val otherBacteria = for (otherBacterium <- localEnvironment.bacteria) yield {
      if (otherBacterium != thisBacterium && (thisBacterium.overlaps(otherBacterium) ||
                                              otherBacterium.overlaps(thisBacterium) ||
                                              thisBacterium.intersects(otherBacterium))) {
        log(s"Handling collision at ($x, $y) between bacteria id ${thisBacterium.id} & ${otherBacterium.id}")
        val u1 = Vector2D.fromMagnitudeAndAngle(thisBacterium.speed, thisBacterium._angle)
        val u2 = Vector2D.fromMagnitudeAndAngle(otherBacterium.speed, otherBacterium._angle)
        val (u_rel, r_rel) = (u1 - u2, thisBacterium.location - otherBacterium.location)
        val n_r_rel = -r_rel
        val (m1, m2) = (thisBacterium.mass, otherBacterium.mass)
        val v1 = u1 - (((2 * m2 / (m1 + m2)) *
                        ((u_rel dot r_rel) /
                         r_rel.normSquared)) * r_rel)
        val v2 = u2 - (((2 * m1 / (m1 + m2)) *
                        (((-u_rel) dot n_r_rel) /
                         n_r_rel.normSquared)) * n_r_rel)
        val (ourSpeed, ourAngle) = (coefficientOfRestitution * v1.norm, v1.angle)
        val (theirSpeed, theirAngle) = (coefficientOfRestitution * v2.norm, v2.angle)
        // Handle hybridisation
        val (bacterium1, bacterium2) =
          (thisBacterium.copy(angle = verify(ourAngle, thisBacterium._angle),
                              initialSpeed = verify(ourSpeed, thisBacterium.speed)),
            otherBacterium.copy(angle = verify(theirAngle, otherBacterium._angle),
                                initialSpeed = verify(theirSpeed, otherBacterium.speed)))
        hybridize(bacterium1, bacterium2) match {
          case Some((newBacterium1, newBacterium2)) =>
            thisBacterium = newBacterium1
            newBacterium2
          case None =>
            thisBacterium = bacterium1
            bacterium2
        }
      }
      else otherBacterium
    }
    (thisBacterium, environment.copy(bacteria = otherBacteria.distinct))
  }

  /**
    * Accounts for rotation of bacterium
    */
  lazy val shape: Circle =
  //@formatter:off
    BuilderStyle.build(new Circle(x, y, maxLinearDimension)) { _.setFill(visiblePaint) }
  //@formatter:on
  lazy val baseShape: Ellipse =
  //@formatter:off
    BuilderStyle.build(new Ellipse(x, y, radiusX, radiusY)) { baseShape => {
      baseShape.getTransforms.add(new Rotate(math.toDegrees(_angle), x, y))
      baseShape.setFill(visiblePaint)
      baseShape.setStroke(Color.BLACK)
    }}
  //@formatter:on

  def drawFlagella(graphics: GraphicsContext): Unit = {
    graphics.setStroke(Color.BLACK)
    val (x1, y1) = (x, y)
    for ((x2, y2) <- flagellarEndCoordinates) {
      graphics.strokeLine(x1, y1, x2, y2)
    }
  }

  def draw(canvas: Canvas): Unit =
    if (screenArea >= environment.screenArea)
      throw new IllegalArgumentException("Cannot draw object larger than environment in environment")
    else {
      val graphics = canvas.getGraphicsContext2D
      graphics.save()
      drawFlagella(graphics)
      val filledSnapshot =
        baseShape.snapshot(ui.View.snapshotParameters, null)
      graphics.drawImage(filledSnapshot,
                         x - filledSnapshot.getWidth / 2,
                         y - filledSnapshot.getHeight / 2)
      graphics.restore()
    }
}

object Bacterium extends UIProxy[Bacterium] with Ordering[Bacterium] {

  override def compare(x: Bacterium, y: Bacterium): Int = x.id compare y.id

  case class LimitingProbabilities(fissionLimitingProbability: Double,
                                   hybridizationLimitingProbability: Double) {
    if (!(fissionLimitingProbability inExact normalIntervalClosed) ||
        !(hybridizationLimitingProbability inExact normalIntervalClosed))
      throw new IllegalArgumentException(s"Limiting probability for binary fission" +
                                         s" $fissionLimitingProbability and" +
                                         s" limiting probability for hybridization " +
                                         s"$hybridizationLimitingProbability" +
                                         s" must belong to $normalIntervalClosed")
  }

  object LimitingProbabilities {
    private val default =
      LimitingProbabilities(0.0, 0.0)

    def apply(): LimitingProbabilities = default
  }

  case class BacteriumParameters(meanRadius: Double,
                                 deviation: Double,
                                 numberOfFlagella: Int,
                                 hungerIncrement: Double,
                                 fissionLimitingHunger: Double,
                                 hybridizationLimitingHunger: Double,
                                 massScale: Double, forceScale: Double, areaScale: Double,
                                 scoringFunction: FoodSourceScoringFunction,
                                 limitingProbabilities: LimitingProbabilities,
                                 unTumbleThreshold: Double,
                                 cannibal_? : Boolean)

  case class FoodSourceScoringFunction(a: Double, b: Double, c: Double) {
    lazy val score: (Double, Double, Double) =>
      Double = (x: Double, y: Double, z: Double) =>
      a * x + b * y + c * z

    def average(other: FoodSourceScoringFunction): FoodSourceScoringFunction =
      FoodSourceScoringFunction((a + other.a) / 2.0, (b + other.b) / 2.0, (c + other.c) / 2.0)
  }

  object FoodSourceScoringFunction {
    // Sensible default, see below
    private val default =
      FoodSourceScoringFunction(0.5, 0.3, 0.2)

    def apply(): FoodSourceScoringFunction = default
  }

  private def nextBooleanWithMaxProbability(maxProbability: Double): Boolean =
    rng.nextBoolean(rng.nextDouble(maxProbability))

  private def safeLimit(size: Dimension2D): Double = size.getWidth max size.getHeight

  def isAlive(bacterium: Bacterium): Boolean = bacterium._hunger < 1 - Epsilon

  case object Defaults {
    val maxOffsetAngle: Double = Pi / 3.0
    val offsetAngleIncrement: Double = 0.1 * maxOffsetAngle
    // Experimentally optimized default values -
    // generating these randomly could mess up the simulation's characteristics
    val unTumbleThreshold = 0.95
    val fissionLimitingHunger = 0.7
    // This one is somewhat arbitrary
    val hybridizationLimitingHunger = 0.7
    // Dependent on viewport resolution (the value below is for 0.85 of 1920x1080 viewports)
    val (minSpeed, maxSpeed) = (100.0, 600.0)
    val maxSpeedDelta: Double = maxSpeed - minSpeed
    // Again, arbitrary - but more makes it look hairy
    val maxFlagella = 20
    // Makes sense for first spawn
    val initialHunger: Double = 0.0
    // This clamping range has been decided from careful optimization,
    // the Boltzmann function behaves weirdly otherwise
    val (minScaledRadius, maxScaledRadius) = (0.1, 1.0)
    // Following are arbitrary
    val size = new Dimension2D(30, 10)
    val scales = Scales(hungerScale = 0.01,
                        massScale = 2,
                        forceScale = 4,
                        areaScale = 1)
    // These are not really defaults, but named constants -
    // they're not meant to be user-configurable
    val (minUnStickValue, maxUnStickValue) = (0.05, 0.95)

    // Can be anything as long as priority of parameters is respected:
    // Priority of =>
    // (closeness to food source) -> Coefficient `a` >
    // (nutrient to toxicity ratio of food source) -> Coefficient `b` >=
    // (quorum "opinion", i.e., the number of bacteria acknowledging this food source to be the closest to them) -> Coefficient `c`
    def scoringFunction: FoodSourceScoringFunction = {
      // Randomly generate scoring coefficients.
      // Guaranteed to be in descending order, as required by priority rules
      val Seq(a, b, c) =
      rng.nextDoublesForProbability(3)
      FoodSourceScoringFunction(a, b, c)
    }

    val limitingProbabilities =
      LimitingProbabilities(hybridizationLimitingProbability = 0.005,
                            fissionLimitingProbability = 0.005)
  }

  case class RotationParameters(rotated_? : Boolean = false,
                                previousAngle: Double = Double.NaN,
                                _offsetAngle: Double = 0.0,
                                _maxOffsetAngle: Double = Defaults.maxOffsetAngle,
                                _offsetAngleIncrement: Double = Defaults.offsetAngleIncrement) {
    val maxOffsetAngle: Double = clamp(0.0, PiBy2)(_maxOffsetAngle)
    val offsetAngle: Double = clampNonNegative(_offsetAngle) % maxOffsetAngle
    val offsetAngleIncrement: Double = clamp(0.0, maxOffsetAngle)(_offsetAngleIncrement)
  }

  case class Scales(hungerScale: Double, massScale: Double, forceScale: Double, areaScale: Double)

  lazy val geneticDistinctionCriteria: (Bacterium) => (Double, FoodSourceScoringFunction) =
    (bacterium: Bacterium) =>
      (bacterium.meanRadius, bacterium.standardParameters.scoringFunction)

  private def randomAngle = rng.nextDouble(PiTimes2)

  private def randomHungerIncrement(scales: Scales, initialSpeed: Double): Double = {
    val verifiedInitialSpeed = verify(initialSpeed, randomSpeed)
    verify(scales.hungerScale, Defaults.scales.hungerScale) *
    rng.nextDouble((verifiedInitialSpeed + (if (verifiedInitialSpeed == Defaults.minSpeed) 1.0 else 0.0) - Defaults.minSpeed) / Defaults.maxSpeedDelta)
  }

  private def randomResponseCoefficient = rng.nextDouble(Defaults.minScaledRadius, Defaults.maxScaledRadius)

  private def randomNumberOfFlagella = 1 + rng.nextInt(Defaults.maxFlagella)

  def randomSpeed: Double = rng.nextDouble(Defaults.minSpeed, Defaults.maxSpeed).round

  def spawnRandomBacterium(id: Int, location: Point, environment: Environment): Bacterium =
    spawnRandomBacterium(id, location, environment, Defaults.size, Defaults.scales)

  def spawnRandomBacterium(id: Int, location: Point, environment: Environment,
                           size: Dimension2D,
                           scales: Scales): Bacterium = {
    val (initialSpeed, initialAngle) =
      (randomSpeed, randomAngle)
    val standardParameters = BacteriumParameters(randomResponseCoefficient,
                                                 randomResponseCoefficient,
                                                 randomNumberOfFlagella,
                                                 randomHungerIncrement(scales, initialSpeed),
                                                 Defaults.fissionLimitingHunger,
                                                 Defaults.hybridizationLimitingHunger,
                                                 scales.massScale, scales.forceScale, scales.areaScale,
                                                 Defaults.scoringFunction,
                                                 Defaults.limitingProbabilities,
                                                 Defaults.unTumbleThreshold,
                                                 /*
                                                 * Note that although the concept of cannibalism is supported,
                                                 * the implementation may be problematic (the system can lose track of intermediates - no trails may be produced for some bacteria),
                                                 * and observationally the bacteria form weird cannibalistic clumps where a nutritional equilibrium gets established.
                                                 *
                                                 * As an artifact of the system, it can also result in "zombie bacteria", bacteria which are displayed but aren't tracked -
                                                 * this is probably due to asynchronous updates and subsequent data invalidation.
                                                 * This becomes a problem when combined with the pause-resume functionality,
                                                 * because sometimes after resuming these zombie bacteria disappear from the display but stay resident in memory. (???)
                                                 *
                                                 * This makes the simulation possibly unending and quite problematic under typical circumstances,
                                                  * hence this is disabled by default.
                                                 * If there is good reason for enabling this, only then enable it, otherwise leave it as-is
                                                 */
                                                 cannibal_? = false)
    Bacterium(id, location,
              size, initialAngle, initialSpeed,
              Defaults.initialHunger, standardParameters, environment)
  }

  def spawnRandomBacterium(id: Int, environment: Environment,
                           size: Dimension2D = Defaults.size,
                           scales: Scales = Defaults.scales): Bacterium =
    spawnRandomBacterium(id, environment.randomPointInside(safeLimit(size)), environment, size, scales)

  def spawnRandomBacteria(number: Int, environment: Environment,
                          size: Dimension2D = Defaults.size,
                          scales: Scales = Defaults.scales): Seq[Bacterium] =
    spawnRandomBacteria(number, environment, IndexedSeq.fill(number)(size), IndexedSeq.fill(number)(scales))

  def spawnRandomBacteria(number: Int, environment: Environment, sizes: Seq[Dimension2D], allScales: Seq[Scales]): Seq[Bacterium] = {
    if (number < 0)
      throw new IllegalArgumentException(s"Number of bacteria to be spawned, $number, cannot be -ve")
    var bacteria = IndexedSeq[Bacterium]()
    while (bacteria.length < number) {
      val currentIndex = bacteria.length
      val bacterium = spawnRandomBacterium(bacteria.length, environment, sizes(currentIndex), allScales(currentIndex))
      if (environment.distinctElement(bacteria, bacterium)) bacteria :+= bacterium
    }
    bacteria
  }
}