package chemotaxis.biology

import javafx.scene._
import canvas.{Canvas, GraphicsContext}
import input.{MouseButton, MouseEvent}
import javafx.stage.Screen
import paint.{Color, CycleMethod, LinearGradient, Stop}
import shape.{Circle, Line}

import chemotaxis._
import extensions._
import Extensions._
import MathUtilities._
import geometry.{CircularBiologicalRestrictedShape, Collisions, RestrictedShape}
import Environment.{Parameters, Statistics}

import language.postfixOps
import scala.collection.SortedMap
import scala.util.{Failure, Success, Try}

/**
  * Represents the culture environment of the bacteria
  */
case class Environment(width: Double, height: Double, center: Point,
                       bacteria: Seq[Bacterium] = IndexedSeq(),
                       foodSources: Seq[FoodSource] = IndexedSeq(),
                       allBacteria: Set[Bacterium] = Set(),
                       allFoodSources: Set[FoodSource] = Set(),
                       parameters: Parameters = Parameters(),
                       statistics: Statistics = Statistics())
  extends CircularBiologicalRestrictedShape {

  import Environment._

  override val environment: Environment = this

  def initializedWith(foodSources: Seq[FoodSource], bacteria: Seq[Bacterium]): Environment = {
    val finalizedEnvironment = copy(bacteria = bacteria,
                                    foodSources = foodSources,
                                    statistics = statistics.copy(_spawnedFoodSources = foodSources.length,
                                                                 _spawnedBacteria = bacteria.length))
    finalizedEnvironment.copy(bacteria = finalizedEnvironment.bacteria.map(_.copy(environment = finalizedEnvironment)),
                              foodSources = finalizedEnvironment.foodSources.map(_.copy(environment = finalizedEnvironment)))
  }

  def initializedWith(foodSourceSpawner: Environment => Seq[FoodSource],
                      bacteriaSpawner: Environment => Seq[Bacterium]): Environment =
    initializedWith(foodSourceSpawner(this), bacteriaSpawner(this))

  import CircularBiologicalRestrictedShape._

  if (width <= 0 || height <= 0)
    throw new IllegalArgumentException(s"Cannot create environment of invalid size: width = $width, height = $height")

  val diameter: Double = width min height
  val radius: Double = (diameter / 2).toInt
  val viscosity: Double = clampNonNegative(isFiniteOrDefault(parameters.mediumViscosity, Defaults.viscosity))
  val viscosityIncrement: Double = clampNonNegative(isFiniteOrDefault(parameters.mediumViscosityIncrement, Defaults.viscosityIncrement))
  private val borderWidth = clampNormalized(isFiniteOrDefault(parameters.borderFactor, Defaults.borderFactor)) * diameter
  val innerDiameter: Double = diameter - borderWidth
  val innerRadius: Double = (innerDiameter / 2).toInt
  val safeWidth, safeHeight = innerDiameter
  val (safeLeftExtreme, safeRightExtreme, safeTopExtreme, safeBottomExtreme) =
    (center.x - innerRadius, center.x + innerRadius,
      center.y - innerRadius, center.y + innerRadius)
  val (leftExtreme, rightExtreme, topExtreme, bottomExtreme) =
    (center.x - radius, center.x + radius,
      center.y - radius, center.y + radius)
  private lazy val (_allBacteria, _allFoodSources) =
    (allBacteria ++ bacteria, allFoodSources ++ foodSources)

  private lazy val backgroundStops =
    Array(new Stop(0, Color.LIGHTSKYBLUE),
          new Stop(0.25, Color.SILVER),
          new Stop(0.5, Color.CORNSILK),
          new Stop(0.75, Color.SILVER),
          new Stop(1.0, Color.LIGHTSLATEGRAY))

  override lazy val visibleColor: Color = Color.WHITE
  override lazy val visiblePaint =
    new LinearGradient(0.0, 0.0, 1.0, 1.0,
                       true, CycleMethod.NO_CYCLE,
                       backgroundStops: _*)

  private lazy val allowedDeviation = diameter * ScreenEpsilon

  override def react(event: MouseEvent): Reaction =
    super.react(event) match {
      case Some(_) =>
        // Bacteria get priority as they move over food sources, not vice-versa
        val elements = (bacteria ++ foodSources).view
        elements.map(_.react(event)).find(_.isDefined) match {
          case Some(reaction) => reaction
          case None =>
            Try(elements.minBy(_.center.distance(event.getSceneX, event.getSceneY))) match {
              case Success(closestElement) =>
                val closestElementShape = closestElement.shape
                val forgivingBoundsOfClosestElement = new Circle(closestElementShape.getCenterX,
                                                                 closestElementShape.getCenterX,
                                                                 closestElementShape.getRadius + allowedDeviation)
                if (Collisions.circleContainsPoint(forgivingBoundsOfClosestElement, (event.getSceneX, event.getSceneY)))
                  closestElement.react(event)
                else event.getEventType match {
                  case MouseEvent.MOUSE_CLICKED =>
                    log(s"Mouse Click event at (${event.getSceneX},${event.getSceneY}), closest element: $closestElement")
                    if (event.getButton == MouseButton.PRIMARY) {
                      log("Primary mouse click, spawning food source")
                      Some(environmentWithNewFoodSourceSpawnedAt(event.getSceneX, event.getSceneY))
                    } else if (event.getButton == MouseButton.SECONDARY) {
                      log("Secondary mouse click, spawning bacterium")
                      Some(environmentWithNewBacteriumSpawnedAt(event.getSceneX, event.getSceneY))
                    } else defaultPositiveReaction
                  case _ => defaultNegativeReaction
                }
              case Failure(_) => defaultNegativeReaction
            }
        }
      case None => defaultNegativeReaction
    }


  def selfConsistent: Environment = {
    val bacterialStatisticalCorrection = (bacteria.length - statistics.liveBacteria).abs
    val foodSourceStatisticalCorrection = (foodSources.length - (statistics.spawnedFoodSources - statistics.consumedFoodSources)).abs
    val consistentStatistics = statistics.copy(_deadBacteria =
                                                 statistics.deadBacteria + bacterialStatisticalCorrection,
                                               _consumedFoodSources =
                                                 statistics.consumedFoodSources + foodSourceStatisticalCorrection)
    copy(statistics = consistentStatistics)
  }

  def environmentWithNewBacteriumSpawnedAt(location: Point): Environment = {
    val newBacterium = Bacterium.spawnRandomBacterium(newBacterialID, location, this)
    val newStatistics = statistics.copy(_spawnedBacteria = statistics.spawnedBacteria + 1)
    copy(bacteria = bacteria :+ newBacterium,
         allBacteria = _allBacteria + newBacterium,
         statistics = newStatistics)
  }

  def environmentWithNewFoodSourceSpawnedAt(location: Point): Environment = {
    val newFoodSource = FoodSource.spawnRandomFoodSource(newFoodSourceID, location, this)
    val newStatistics = statistics.copy(_spawnedFoodSources = statistics.spawnedFoodSources + 1)
    copy(foodSources = foodSources :+ newFoodSource,
         allFoodSources = _allFoodSources + newFoodSource,
         statistics = newStatistics)
  }

  lazy val shape: Circle =
  //@formatter:off
    BuilderStyle.build(new Circle(center.x, center.y, innerRadius)) { _.setFill(visiblePaint) }
  //@formatter:on

  private lazy val maskRegion =
  //@formatter:off
    BuilderStyle.build(new Circle(center.x, center.y, radius)) { _.setFill(visiblePaint) }
  //@formatter:on

  private def randomPointInsideBoundingRectangle(safeBoundary: Circle): Point = {
    val bounds = safeBoundary.getBoundsInParent
    (bounds.getMinX + rng.nextInt(bounds.getWidth.toInt),
      bounds.getMinY + rng.nextInt(bounds.getHeight.toInt)).roundedDown
  }

  def randomPointInside(safeRegion: Double): Point = {
    val safeBoundary = new Circle(shape.getCenterX, shape.getCenterY, shape.getRadius - safeRegion)
    var point = randomPointInsideBoundingRectangle(safeBoundary)
    while (!Collisions.circleContainsPoint(safeBoundary, point)) {
      point = randomPointInsideBoundingRectangle(safeBoundary)
    }
    point.roundedDown
  }

  override def within(point: Point): Boolean =
    Collisions.circleContainsPoint(maskRegion, point)

  def lineIntersectsBoundary(line: Line): Option[Point] =
    Geometry.lineIntersectsCircle(line, shape)

  lazy val withoutLocationHistory: Environment =
    this.copy(statistics = this.statistics.copy(locationHistory = SortedMap[Bacterium, Seq[LocationInfo]]()(Bacterium)))

  def updated(time: Double): Environment = {
    var currentEnvironment = copy()
    //log("Updating state")
    val newBacteria = (for (bacterium <- bacteria) yield {
      //log(bacterium)
      val (newEnvironment, newTemporaryBacteria) =
        bacterium.copy(environment = currentEnvironment).move(time)
      val newStatistics = newEnvironment.statistics
      val (newBacteriaTotal, newBacteriaCount) = (newTemporaryBacteria.length, newTemporaryBacteria.count(_.isDefined))
      val spawnedBacteria = if (newBacteriaTotal > 1) newBacteriaCount else 0
      val killedBacteria = newBacteriaTotal - newBacteriaCount
      val modifiedStatistics =
        newStatistics.copy(_deadBacteria = newStatistics.deadBacteria + killedBacteria,
                           _spawnedBacteria = newStatistics.spawnedBacteria + spawnedBacteria)
      currentEnvironment = newEnvironment.copy(allFoodSources = newEnvironment._allFoodSources,
                                               statistics = modifiedStatistics)
      newTemporaryBacteria
    }).flatten
    //@formatter:off
    val liveBacteria = newBacteria.collect { case Some(bacterium) => bacterium }
    //@formatter:on
    val realLiveBacteria = liveBacteria.filter(Bacterium.isAlive)
    currentEnvironment.copy(bacteria = realLiveBacteria,
                            allBacteria = currentEnvironment._allBacteria ++ liveBacteria,
                            // We note that the viscosity of the agar increases as it solidifies due to evaporation of moisture,
                            // and it settling down due to gravity.
                            parameters = currentEnvironment.parameters
                              .copy(mediumViscosity =
                                      currentEnvironment.viscosity + currentEnvironment.viscosityIncrement),
                            statistics = currentEnvironment.statistics
                              .copy(locationHistory = currentEnvironment.locationHistoryUpdated,
                                    _deadBacteria =
                                      currentEnvironment.statistics.deadBacteria + (liveBacteria.length - realLiveBacteria.length),
                                    _geneticallyUniqueBacteria =
                                      liveBacteria.distinctBy(bacterium => (bacterium.meanRadius, bacterium.deviation)).length,
                                    _stoppedBacteria =
                                      liveBacteria.count(_.stopped),
                                    _maxGeneration =
                                      Try(liveBacteria.maxBy(_.generation)).map(_.generation)
                                        .getOrElse(currentEnvironment.statistics.maxGeneration)))
      .copy(foodSources = currentEnvironment.foodSources.map(_.copy(environment = currentEnvironment)))
  }

  //@formatter:off
  def locationHistoryUpdated: LocationHistory =
    bacteria.foldLeft(statistics.locationHistory) { (locationHistory, bacterium) => {
      val (location, color) = bacterium.locationInfo
      locationHistory.get(bacterium) match {
        case Some(points) =>
          val LocationInfo(_, lastLocation, _) = points.last
          locationHistory.updated(bacterium, points :+ LocationInfo(lastLocation, location, color))
        case None =>
          locationHistory + (bacterium -> IndexedSeq(LocationInfo(location, location, color)))
      }}}
  //@formatter:on

  def newBacterialIDs(numOfBacteria: Int): Set[Int] =
    newIDs(numOfBacteria, _allBacteria.maxBy(_.id).id)

  def newBacterialID: Int =
    newBacterialIDs(1).head

  def newFoodSourceIDs(numOfFoodSources: Int): Set[Int] =
    newIDs(numOfFoodSources, _allFoodSources.maxBy(_.id).id)

  def newFoodSourceID: Int =
    newFoodSourceIDs(1).head

  def distinctElement[A <: RestrictedShape](elements: Seq[A], element: A): Boolean =
    !elements.exists(existingElement => existingElement == element ||
                                        existingElement.intersects(element) ||
                                        ColorUtilities.perceptuallySimilar(existingElement.visibleColor, element.visibleColor)) && overlaps(element)

  def drawTrails(canvas: GraphicsContext): Unit = {
    canvas.save()
    /* Using a SortedMap ensures that the later bacteria have their trails drawn on top */
    //log("Drawing trails")
    //noinspection ScalaUnusedSymbol
    for ((individual, individualHistory) <- statistics.locationHistory) {
      //log(individual)
      for (LocationInfo((x1, y1), (x2, y2), segmentColor) <- individualHistory) {
        canvas.setStroke(segmentColor)
        canvas.strokeLine(x1, y1, x2, y2)
      }
    }
    canvas.restore()
  }

  def drawBase(canvas: Canvas): Unit = {
    if ((canvas.getHeight min canvas.getWidth) < radius) {
      throw new IllegalArgumentException("Cannot draw environment in canvas smaller than environment")
    }
    val graphics = canvas.getGraphicsContext2D
    graphics.save()
    // Paint Glass-like background of plate
    graphics.setFill(visiblePaint)
    graphics.fillOval(safeLeftExtreme, safeTopExtreme, innerDiameter, innerDiameter)
    graphics.restore()
    graphics.save()
  }

  def draw(canvas: Canvas): Unit = {
    drawBase(canvas)
    val graphics = canvas.getGraphicsContext2D
    //draw contents
    foodSources foreach (_ draw canvas)
    drawTrails(graphics)
    //log("Drawing bacteria")
    //bacteria foreach log
    bacteria foreach (_ draw canvas)
    //draw wall of plate
    graphics.setStroke(Color.SILVER)
    graphics.setLineWidth(borderWidth)
    graphics.strokeOval(leftExtreme, topExtreme, diameter, diameter)
    //Mask the central region so that we don't get bits and pieces outside the plate
    canvas.setClip(maskRegion)
    graphics.restore()
  }
}

object Environment extends UIProxy[Environment] {

  case class Parameters(mediumViscosity: Double = Defaults.viscosity,
                        mediumViscosityIncrement: Double = Defaults.viscosityIncrement,
                        borderFactor: Double = Defaults.borderFactor)

  case class LocationInfo(lastLocation: Point, location: Point, color: Color)

  type LocationHistory = SortedMap[Bacterium, Seq[LocationInfo]]

  case class Statistics(_deadBacteria: Int,
                        _spawnedBacteria: Int,
                        _consumedFoodSources: Int,
                        _spawnedFoodSources: Int,
                        _geneticallyUniqueBacteria: Int,
                        _stoppedBacteria: Int,
                        _maxGeneration: Int,
                        locationHistory: LocationHistory) {
    val maxGeneration: Int = clampNatural(_maxGeneration)
    val spawnedBacteria: Int =
      clampAbove(clampNonNegative(_deadBacteria)).apply(_spawnedBacteria)
    val deadBacteria: Int =
      clamp(0, spawnedBacteria)(_deadBacteria)
    val liveBacteria: Int =
      spawnedBacteria - deadBacteria
    val geneticallyUniqueBacteria: Int =
      clamp(0, liveBacteria)(_geneticallyUniqueBacteria)
    val stoppedBacteria: Int =
      clamp(0, liveBacteria)(_stoppedBacteria)
    val spawnedFoodSources: Int =
      clampAbove(clampNonNegative(_consumedFoodSources)).apply(_spawnedFoodSources)
    val consumedFoodSources: Int =
      clamp(0, spawnedFoodSources)(_consumedFoodSources)
    lazy val allBacteriaDead_? : Boolean = liveBacteria == 0
    lazy val allBacteriaStopped_? : Boolean = stoppedBacteria == liveBacteria
    lazy val allBacteriaStoppedButNotDead_? : Boolean = allBacteriaStopped_? && (!allBacteriaDead_?)
    lazy val allFoodSourcesConsumed_? : Boolean = consumedFoodSources == spawnedFoodSources
  }

  object Statistics {
    private val default = Statistics(_deadBacteria = 0,
                                     _spawnedBacteria = 0,
                                     _consumedFoodSources = 0,
                                     _spawnedFoodSources = 0,
                                     _geneticallyUniqueBacteria = 0,
                                     _stoppedBacteria = 0,
                                     _maxGeneration = 1,
                                     locationHistory = SortedMap[Bacterium, Seq[LocationInfo]]()(Bacterium))

    def apply(): Statistics = default

    def onlySpawned(spawnedBacteria: Int, spawnedFoodSources: Int): Statistics = {
      if (spawnedBacteria < 0)
        throw new IllegalArgumentException(s"Number of spawned bacteria cannot be negative, is $spawnedBacteria")
      if (spawnedFoodSources < 0)
        throw new IllegalArgumentException(s"Number of spawned food sources cannot be negative, is $spawnedFoodSources")
      default.copy(_spawnedBacteria = spawnedBacteria,
                   _spawnedFoodSources = spawnedFoodSources,
                   _geneticallyUniqueBacteria = spawnedBacteria)
    }
  }

  case object Defaults {
    val (xScale, yScale) = (0.95, 0.95)
    val (viscosity, density) = (agarViscositySI, agarDensitySI)
    val viscosityIncrement: Double = ScreenEpsilon * viscosity
    val borderFactor: Double = 0.01
  }

  def createBare(width: Double, height: Double): Environment =
    Environment((Defaults.xScale * width).toInt, (Defaults.yScale * height).toInt, (width / 2, height / 2))

  def createRandomized(width: Double, height: Double,
                       foodSourceSpawner: Environment => Seq[FoodSource],
                       bacteriaSpawner: Environment => Seq[Bacterium]): Environment =
    Environment.createBare(width, height).initializedWith(foodSourceSpawner, bacteriaSpawner)

  /**
    * Density of agar (d) = 340kg/m^3^
    * Dynamic viscosity of agar (u) = 45 cP =  0.045 Pa.s
    * Thus, Kinematic viscosity of agar(v) = v = u/d = 0.000132352941
    */
  val (agarViscositySI, agarDensitySI) =
    (0.0045, 340)

  val pixelsPerMeter: Double =
    Screen.getPrimary.getDpi /*dpi of screen*/ * 39.37 /*inches in a meter*/

  private def newIDs(numOfIDs: Int, startingID: Int): Set[Int] = numOfIDs match {
    case 0 =>
      Set()
    case _ =>
      (1 to clampNatural(numOfIDs))
        .map(startingID +).toSet
  }
}
