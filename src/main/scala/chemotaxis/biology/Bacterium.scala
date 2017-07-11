package chemotaxis.biology

import javafx.geometry.Dimension2D
import javafx.scene._
import input.MouseEvent
import canvas.{Canvas, GraphicsContext}
import paint.Color
import shape.{Circle, Ellipse}
import transform.Rotate

import chemotaxis._
import biology.Bacterium._
import extensions.Extensions._
import extensions.MathUtilities._

/**
  * Represents one bacterium
  */
case class Bacterium(id: Int, location: Point,
                     size: Dimension2D, angle: Double,
                     initialSpeed: Double,
                     hunger: Double,
                     standardParameters: BacteriumParameters,
                     environment: Plate, color: Option[Color] = None,
                     rotationParameters: RotationParameters =
                     RotationParameters())
  extends geometry.CircularBiologicalRestrictedShape {
  def toSerializableString: String = ??? //TODO: Implement serialization support

  override val center: Point = location

  override def react(event: MouseEvent): Boolean = {
    if (super.react(event)) {
      // TODO: Implement something here if necessary
      if (event.getEventType == MouseEvent.MOUSE_CLICKED) {
        println(s"Mouse Click on bacterium ID $id," +
                s" click location: (${event.getSceneX},${event.getSceneY})," +
                s" element location: ($location)," +
                s" element: $this")
      }
      true
    } else false
  }

  if (!environment.within(location))
    throw new IllegalArgumentException(s"$this spawned outside of environment")
  private lazy val _hunger =
    clampNormalized(hunger)
  private lazy val hungerIncrement =
    clamp(0, 1 - _hunger)(standardParameters.hungerIncrement)
  private lazy val areaScale =
    clampNonNegative(standardParameters.areaScale)
  private lazy val fissionLimitingHunger =
    clampNormalized(standardParameters.fissionLimitingHunger)
  private lazy val hybridizationLimitingHunger =
    clampNormalized(standardParameters.hybridizationLimitingHunger)
  lazy val speed: Double =
    clampNonNegative(if (initialSpeed.isNaN) randomSpeed else initialSpeed)
  private lazy val unTumbleThreshold =
    clampNormalized(standardParameters.unTumbleThreshold)
  lazy val scaledSpeed: Double =
    speed / Plate.pixelsPerMeter
  // For collision velocity calculations, and other things...
  lazy val mass: Double =
    clampNatural(standardParameters.massScale) * (1 - _hunger)
  lazy val force: Double =
    clampNatural(standardParameters.forceScale) * (1 - _hunger) - drag

  private def normalizeAngle(angle: Double): Double = {
    val normalizedAngle = angle % PiTimes2
    (if (normalizedAngle < 0.0) PiTimes2 else 0.0) + normalizedAngle
  }

  private lazy val _angle = normalizeAngle(angle)
  private lazy val previousAngle = if (rotationParameters.previousAngle.isNaN) _angle else normalizeAngle(rotationParameters.previousAngle)
  lazy val responseCoefficient: Double =
    clampNonNegative(standardParameters.responseCoefficient)
  val maxDistance: Double =
    environment.diameter
  private lazy val flagella =
    clampNatural(standardParameters.numberOfFlagella)

  private lazy val defaultColor = Color.hsb(responseCoefficient * 360, 1.0, 1.0)
  // Just a way to get a color dependent on a unique constant property of a bacterium
  override lazy val visibleColor: Color = color.getOrElse(defaultColor)
  private lazy val internalArea = areaScale * mass
  private lazy val lengthScale = math.sqrt(internalArea) * 0.5
  private lazy val (width, height) = (clampNonNegative(size.getWidth), clampNatural(size.getHeight))
  lazy val (radiusX, radiusY) = (lengthScale * (width, height)).roundedDown
  private lazy val diagonal = math.hypot(width, height).round
  private lazy val flagellumLength = (lengthScale * diagonal).round
  private lazy val maxLinearDimension = 2 * flagellumLength
  private lazy val coefficientOfRestitution =
    (maxLinearDimension / environment.bacteria.map(_.maxLinearDimension).max) * (flagella / environment.bacteria.map(_.flagella).max.toDouble)

  private val (x, y) = location.roundedDown

  private val probabilityScaleFactor = math.E * responseCoefficient

  override def equals(obj: scala.Any): Boolean =
    super.equals(obj) || (obj match {
      case other: Bacterium => id == other.id
      case _ => false
    })

  lazy val stopped: Boolean = speed < ScreenEpsilon
  override lazy val toString: String =
    s"${getClass.getSimpleName}[" +
    s"ID = $id, location = ($x, $y)," +
    s" size = $size" +
    s" color = (provided = $color, current = $visiblePaint)," +
    s" flagella = $flagella (length = $flagellumLength)," +
    s" speed = $speed, response = $responseCoefficient," +
    s" un-tumble threshold = $unTumbleThreshold, angle = $angle," +
    s" mass = (${standardParameters.massScale}, $mass)" +
    s" area = (${standardParameters.areaScale}, X radius = $radiusX, Y radius = $radiusY)" +
    s" hunger = (base = ${_hunger}, increment = $hungerIncrement)," +
    s" limiting hunger =" +
    s" (fission = $fissionLimitingHunger," +
    s" hybridization = $hybridizationLimitingHunger)," +
    s" force = (${standardParameters.forceScale}, $force)," +
    s" food source scoring function = ${standardParameters.scoringFunction}]"

  override lazy val hashCode: Int = id.##

  private def fed(plate: Plate, bacterium: Bacterium): (Boolean, Plate, Option[Bacterium]) = {
    val feeding = plate.foodSources
      .filter(FoodSource.isExistent)
      // We need the bacteria to be on the food before it can consume it -
      // bacteria feeding by surface absorption is not a baseless assumption.
      // Flagella do not contribute.
      .findWithIndex {
                       // Side Effect:
                       // The smaller a bacterium becomes (due to size-hunger scaling), the smaller food sources it can consume
                       // i.e., the more hungry a bacterium, the more desperate it is to extract nutrition even from low-quality sources
                       foodSource =>
                         (foodSource overlaps bacterium.baseShape) ||
                         bacterium.baseShape.getBoundsInParent.contains(foodSource.shape.getBoundsInParent)
                     }
    // if the bacterium has reached the food, and it is hungry
    val (didFeed, reduceHunger, newFoodSources) =
      if (feeding.isDefined && bacterium._hunger.abs >= Epsilon) {
        // it will consume the food
        val (foodSource, index) = feeding.get
        val reducedFoodSource = foodSource.consumed(bacterium._hunger)
        // Linear relationship here between hunger, food consumed and toxicity
        val reduceHunger = (1 - (reducedFoodSource._amount / foodSource._amount)) *
                           (1 + foodSource.toxicityRatio) * bacterium._hunger
        val newFoodSources = plate.foodSources.updated(index, reducedFoodSource)
        (true, reduceHunger, newFoodSources)
      } else (false, 0.0, plate.foodSources)
    val newHunger = bacterium._hunger - reduceHunger
    val existingFoodSources = newFoodSources.filter(FoodSource.isExistent)
    val currentPlateStatistics = plate.plateStatistics
    val newPlateStatistics =
      currentPlateStatistics
        .copy(_consumedFoodSources =
                currentPlateStatistics.consumedFoodSources +
                (newFoodSources.length - existingFoodSources.length))
    val newEnvironment = plate.copy(foodSources = existingFoodSources,
                                    plateStatistics = newPlateStatistics)
    if (newHunger < 1 - Epsilon)
      (didFeed, newEnvironment, Some(bacterium.copy(hunger = newHunger)))
    else
      (true, newEnvironment, None)
  }

  private val calcNormalizedDistanceTo: (FoodSource) => Double =
    (foodSource: FoodSource) =>
      location.distance(foodSource.location) / maxDistance

  private def hybridize(bacterium1: Bacterium, bacterium2: Bacterium):
  Option[(Bacterium, Bacterium)] = {
    lazy val willHybridize =
      nextBooleanWithMaxProbability(standardParameters.limitingProbabilities.hybridizationLimitingProbability)
    if (bacterium1._hunger < bacterium1.hybridizationLimitingHunger &&
        bacterium2._hunger < bacterium2.hybridizationLimitingHunger &&
        willHybridize) {
      val averageResponseCoefficient =
        (bacterium1.responseCoefficient + bacterium2.responseCoefficient) / 2.0
      val averageScoringFunction =
        bacterium1.standardParameters.scoringFunction.average(bacterium2.standardParameters.scoringFunction)
      val averageNumberOfFlagella =
        (bacterium1.flagella + bacterium2.flagella) / 2
      Some((bacterium1.copy(standardParameters =
                              bacterium1.standardParameters
                                .copy(responseCoefficient = averageResponseCoefficient,
                                      scoringFunction = averageScoringFunction,
                                      numberOfFlagella = averageNumberOfFlagella),
                            color = Some(bacterium1.visibleColor)),
             bacterium2.copy(standardParameters =
                               bacterium2.standardParameters
                                 .copy(responseCoefficient = averageResponseCoefficient,
                                       scoringFunction = averageScoringFunction,
                                       numberOfFlagella = averageNumberOfFlagella),
                             color = Some(bacterium2.visibleColor))))
    } else None
  }

  private def divide(bacterium: Bacterium):
  Option[(Bacterium, Bacterium)] = {
    lazy val willDivide =
      nextBooleanWithMaxProbability(standardParameters.limitingProbabilities.fissionLimitingProbability)
    if (_hunger < fissionLimitingHunger && willDivide) {
      val offsetAngle =
      // ϵ (0.0, 1.0), so exclude the endpoints to a reasonable degree
        random.nextDouble(Defaults.minUnStickValue, Defaults.maxUnStickValue) * PiBy2
      val newHunger = 0.5 * (1 + _hunger)
      val ids = environment.newBacterialIDs(2)
      val locationOffset =
        ((if (x > environment.center.x) -1 else 1) * size.getWidth,
          (if (y > environment.center.y) -1 else 1) * size.getHeight)
      val childLocations = IndexedSeq(location + locationOffset, location - locationOffset)
      val allowedChildLocations = childLocations.filter(environment.within)
      if (allowedChildLocations == childLocations) {
        Some((copy(location = allowedChildLocations(0),
                   id = ids.head,
                   angle = _angle + offsetAngle,
                   initialSpeed = speed * math.sin(offsetAngle),
                   hunger = newHunger),
               copy(location = allowedChildLocations(1),
                    id = ids.last,
                    angle = _angle - offsetAngle,
                    initialSpeed = speed * math.cos(offsetAngle),
                    hunger = newHunger)))
      }
      else None
    } else None
  }

  lazy val withoutEnvironment: Bacterium =
    copy(environment = Plate.createBarePlate(environment.width, environment.height))

  private def quorum(foodSource: FoodSource): Int =
    environment.bacteria
      .filter(_ != this)
      .count(_.closestFoodSource == foodSource)

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
  private def scoreSource(foodSource: FoodSource) =
    standardParameters.scoringFunction.score(// We want to maximize closeness, not distance
                                             1 - calcNormalizedDistanceTo(foodSource),
                                             foodSource.nutrientToToxicityRatio,
                                             quorum(foodSource))

  def closestFoodSource: Option[FoodSource] =
    environment.foodSources
      .sortBy(calcNormalizedDistanceTo)
      .headOption

  def move(time: Double): (Plate, Seq[Option[Bacterium]]) = {
    val (bacterium, plate) = handleCollisions(environment)
    fed(plate, bacterium) match {
      case (didFeed, newEnvironment, newBacterium) if didFeed =>
        (newEnvironment, Seq(newBacterium.map(raw => {
          import raw.rotationParameters._
          raw.copy(initialSpeed = speed,
                   environment = newEnvironment,
                   rotationParameters = raw.rotationParameters.copy(rotated_? = false,
                                                                    _offsetAngle = offsetAngle + offsetAngleIncrement))
        })))
      case (_, thisEnvironment, Some(thisBacterium)) =>
        val unaltered = (thisEnvironment, Seq(Some(thisBacterium)))
        // Handle binary fission
        divide(thisBacterium) match {
          case Some((offspring1, offspring2)) =>
            (thisEnvironment,
              Seq(Some(offspring1),
                  Some(offspring2),
                  None))
          case None =>
            val r = thisEnvironment.foodSources
              .sortWith(scoreSource(_) > scoreSource(_))
              .headOption.map(calcNormalizedDistanceTo)
              .getOrElse(1.0)
            val rawTumbleProbability = (thisBacterium.probabilityScaleFactor * r.squared *
                                        math.exp(-thisBacterium.responseCoefficient * r.squared)).min(1 - Epsilon)
            // Ensure that the bacterium doesn't get stuck away from food
            val tumbleProbability = rawTumbleProbability *
                                    (if (math.abs(rawTumbleProbability - unTumbleThreshold) > Epsilon)
                                     // Sensible scaling
                                       random.nextDouble(Defaults.minUnStickValue, Defaults.maxUnStickValue)
                                     else 1.0)
            val willTumble = random.nextBoolean(tumbleProbability)
            // angle to tumble through
            val tumbleDirection = if (willTumble) random.nextDouble(PiTimes2) else 0.0
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
            import thisBacterium.rotationParameters._
            val newRotationParameters = thisBacterium.rotationParameters
              .copy(rotated_? = willTumble,
                    previousAngle = newAngle,
                    _offsetAngle = offsetAngle + offsetAngleIncrement)
            if (pointOfIntersectionWithBoundary.isDefined) {
              val reflectedLocation = pointOfIntersectionWithBoundary.get.rounded
              val currentVelocity = Vector2D.fromMagnitudeAndAngle(newSpeed, newAngle)
              val surfaceNormal = reflectedLocation - thisEnvironment.center
              val currentVelocityProjectedOnSurfaceNormal = surfaceNormal.projectionOf(currentVelocity)
              val reflectedVelocity = currentVelocity - (2 * currentVelocityProjectedOnSurfaceNormal)
              val (reflectedAngle, reflectedSpeed) = (reflectedVelocity.angle, reflectedVelocity.norm)
              // Safeguards
              if (thisEnvironment.within(reflectedLocation))
                (thisEnvironment, Seq(Some(thisBacterium.copy(location = reflectedLocation, angle = reflectedAngle,
                                                              initialSpeed = if (reflectedSpeed.isNaN) speed else reflectedSpeed, hunger = newHunger,
                                                              environment = thisEnvironment, rotationParameters = newRotationParameters.copy(previousAngle = reflectedAngle)))))
              else unaltered
            }
            else {
              // Safeguards
              if (thisEnvironment.within(newLocation))
                (thisEnvironment, Seq(Some(thisBacterium.copy(location = newLocation, angle = newAngle,
                                                              initialSpeed = if (newSpeed.isNaN) speed else newSpeed, hunger = newHunger,
                                                              environment = thisEnvironment, rotationParameters = newRotationParameters))))
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
    Seq.tabulate(flagella) { flagellumIndex => {
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

  def handleCollisions(localEnvironment: Plate): (Bacterium, Plate) = {
    var thisBacterium = copy()
    val otherBacteria = for (otherBacterium <- localEnvironment.bacteria) yield {
      if (otherBacterium != thisBacterium && thisBacterium.intersects(otherBacterium)) {
        val u1 = Vector2D.fromMagnitudeAndAngle(thisBacterium.speed, thisBacterium._angle)
        val u2 = Vector2D.fromMagnitudeAndAngle(otherBacterium.speed, otherBacterium._angle)
        val (u_rel, r_rel) = (u1 - u2, thisBacterium.location - otherBacterium.location)
        val n_r_rel = -r_rel
        val (m1, m2) = (thisBacterium.mass, otherBacterium.mass)
        val v1 = u1 -
                 (((2 * m2 /
                    (m1 + m2)) *
                   ((u_rel dot r_rel) /
                    r_rel.normSquared)) *
                  r_rel)
        val v2 = u2 - (((2 * m1 /
                         (m1 + m2)) *
                        (((-u_rel) dot n_r_rel) /
                         n_r_rel.normSquared)) *
                       n_r_rel)
        val (ourSpeed, ourAngle) = (coefficientOfRestitution * v1.norm, v1.angle)
        val (theirSpeed, theirAngle) = (coefficientOfRestitution * v2.norm, v2.angle)
        // Handle hybridisation
        val (bacterium1, bacterium2) =
          (thisBacterium.copy(initialSpeed = if (ourSpeed.isNaN) speed else ourSpeed, angle = ourAngle),
            otherBacterium.copy(initialSpeed = if (theirSpeed.isNaN) speed else theirSpeed, angle = theirAngle))
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
    (thisBacterium, environment.copy(bacteria = otherBacteria))
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

object Bacterium {

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

  case class BacteriumParameters(responseCoefficient: Double,
                                 numberOfFlagella: Int,
                                 hungerIncrement: Double,
                                 fissionLimitingHunger: Double,
                                 hybridizationLimitingHunger: Double,
                                 massScale: Double, forceScale: Double, areaScale: Double,
                                 scoringFunction: FoodSourceScoringFunction,
                                 limitingProbabilities: LimitingProbabilities,
                                 unTumbleThreshold: Double)

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

  private val random = ui.View.random // Don't specify the type here - makes the code more flexible

  private def nextBooleanWithMaxProbability(maxProbability: Double): Boolean =
    random.nextBoolean(random.nextDouble(maxProbability))

  private def safeLimit(size: Dimension2D): Double = size.getWidth max size.getHeight

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
    val (minResponseCoefficient, maxResponseCoefficient) = (0.1, 1.0)
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
      random.nextDoublesForProbability(3)
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
      (bacterium.responseCoefficient, bacterium.standardParameters.scoringFunction)

  def spawnRandomBacterium(id: Int, location: Point, plate: Plate): Bacterium =
    spawnRandomBacterium(id, location, plate, Defaults.size, Defaults.scales)

  def spawnRandomBacterium(id: Int, location: Point, plate: Plate,
                           size: Dimension2D,
                           scales: Scales): Bacterium = {
    val (initialSpeed, initialAngle) =
      (randomSpeed, random.nextDouble(PiTimes2))

    val hungerIncrement = scales.hungerScale *
                          random.nextDouble((initialSpeed + (if (initialSpeed == Defaults.minSpeed) 1.0 else 0.0) - Defaults.minSpeed) / Defaults.maxSpeedDelta)
    val standardParameters = BacteriumParameters(random.nextDouble(Defaults.minResponseCoefficient, Defaults.maxResponseCoefficient),
                                                 1 + random.nextInt(Defaults.maxFlagella),
                                                 hungerIncrement,
                                                 Defaults.fissionLimitingHunger,
                                                 Defaults.hybridizationLimitingHunger,
                                                 scales.massScale, scales.forceScale, scales.areaScale,
                                                 Defaults.scoringFunction,
                                                 Defaults.limitingProbabilities,
                                                 Defaults.unTumbleThreshold)
    Bacterium(id, location,
              size, initialAngle, initialSpeed,
              Defaults.initialHunger, standardParameters, plate)
  }

  def spawnRandomBacterium(id: Int, plate: Plate,
                           size: Dimension2D = Defaults.size,
                           scales: Scales = Defaults.scales): Bacterium =
    spawnRandomBacterium(id, plate.randomPointInside(safeLimit(size)), plate, size, scales)

  def spawnRandomBacteria(number: Int, plate: Plate,
                          size: Dimension2D = Defaults.size,
                          scales: Scales = Defaults.scales): Seq[Bacterium] =
    spawnRandomBacteria(number, plate, Seq.fill(number)(size), Seq.fill(number)(scales))

  def spawnRandomBacteria(number: Int, plate: Plate, sizes: Seq[Dimension2D], allScales: Seq[Scales]): Seq[Bacterium] = {
    if (number < 0)
      throw new IllegalArgumentException(s"Number of bacteria to be spawned, $number, cannot be -ve")
    var bacteria = IndexedSeq[Bacterium]()
    while (bacteria.length < number) {
      val currentIndex = bacteria.length
      val bacterium = spawnRandomBacterium(bacteria.length, plate, sizes(currentIndex), allScales(currentIndex))
      if (plate.distinctElement(bacteria, bacterium)) bacteria :+= bacterium
    }
    bacteria
  }

  def randomSpeed: Double = random.nextDouble(Defaults.minSpeed, Defaults.maxSpeed).round

  def fromSerializableString(serializedString: String): Bacterium = ??? // TODO: Allow deserialization
}