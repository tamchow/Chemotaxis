package chemotaxis.biology

import javafx.scene._
import canvas.{Canvas, GraphicsContext}
import input.MouseEvent
import javafx.stage.Screen
import paint.{Color, CycleMethod, LinearGradient, Stop}
import shape.{Circle, Line}

import chemotaxis._
import biology.Plate._
import extensions._
import Extensions._
import MathUtilities._
import geometry.{CircularBiologicalRestrictedShape, Collisions, RestrictedShape}

import language.postfixOps

/**
  * Represents the culture environment of the bacteria
  */
case class Plate(width: Double, height: Double, center: Point,
                 bacteria: Seq[Bacterium] = Seq(),
                 foodSources: Seq[FoodSource] = Seq(),
                 allBacteria: Set[Bacterium] = Set(),
                 allFoodSources: Set[FoodSource] = Set(),
                 parameters: Parameters = Parameters(),
                 plateStatistics: PlateStatistics = PlateStatistics())
  extends CircularBiologicalRestrictedShape {

  def toSerializableString: String = ??? //TODO: Implement serialization support

  def initializedWith(foodSources: Seq[FoodSource], bacteria: Seq[Bacterium]): Plate = {
    val finalizedEnvironment = copy(bacteria = bacteria,
                                    foodSources = foodSources,
                                    plateStatistics =
                                      plateStatistics.copy(_spawnedFoodSources = foodSources.length,
                                                           _spawnedBacteria = bacteria.length))
    finalizedEnvironment.copy(bacteria = finalizedEnvironment.bacteria.map(_.copy(environment = finalizedEnvironment)),
                              foodSources = finalizedEnvironment.foodSources.map(_.copy(environment = finalizedEnvironment)))
  }

  def initializedWith(foodSourceSpawner: Plate => Seq[FoodSource],
                      bacteriaSpawner: Plate => Seq[Bacterium]): Plate =
    initializedWith(foodSourceSpawner(this), bacteriaSpawner(this))

  override def react(event: MouseEvent): Boolean = {
    // Bacteria get priority as they move over food sources, not vice-versa
    val elements = bacteria ++ foodSources
    val closestElement = elements.minBy(_.center.distance(event.getSceneX, event.getSceneY))
    val allowedDeviation = diameter * ScreenEpsilon
    val closestElementShape = closestElement.shape
    val forgivingBoundsOfClosestElement = new Circle(closestElementShape.getCenterX, closestElementShape.getCenterX, closestElementShape.getRadius + allowedDeviation)
    val handled = (super.react(event) &&
                  elements.foldLeft(false)((acc, element) => acc || element.react(event))) ||
                  (Collisions.circleContainsPoint(forgivingBoundsOfClosestElement, (event.getSceneX, event.getSceneY)) && closestElement.react(event))
    if (!handled && event.getEventType == MouseEvent.MOUSE_CLICKED) {
      // TODO:  Implement something here if necessary
      println(s"Untargeted Mouse Click event at (${event.getSceneX},${event.getSceneY}) on plate, closest element's shape: ${closestElement.shape}")
    }
    handled
  }

  val diameter: Double = width min height
  val radius: Double = (diameter / 2).toInt
  val density: Double = clampNonNegative(parameters.mediumDensity)
  val viscosity: Double = clampNonNegative(parameters.mediumViscosity)
  val viscosityIncrement: Double = clampNonNegative(parameters.mediumViscosityIncrement)
  val kinematicViscosity: Double = viscosity / density
  private val borderWidth = parameters.borderFactor * diameter
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
    (bounds.getMinX + random.nextInt(bounds.getWidth.toInt),
      bounds.getMinY + random.nextInt(bounds.getHeight.toInt)).roundedDown
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

  lazy val withoutLocationHistory: Plate =
    this.copy(plateStatistics = this.plateStatistics.copy(locationHistory = Map()))

  def updated(time: Double): Plate = {
    var plate = copy()
    val newBacteria = (for (bacterium <- bacteria) yield {
      val (newPlate, newTemporaryBacteria) =
        bacterium.copy(environment = plate).move(time)
      val newPlateStatistics = newPlate.plateStatistics
      val spawnedBacteria = if (newTemporaryBacteria.length > 1) newTemporaryBacteria.count(_.isDefined) else 0
      val modifiedPlateStatistics =
        newPlateStatistics.copy(_deadBacteria =
                                  newPlateStatistics.deadBacteria + newTemporaryBacteria.count(_.isEmpty),
                                _spawnedBacteria =
                                  newPlateStatistics.spawnedBacteria + spawnedBacteria)
      plate = newPlate.copy(allBacteria =
                              newPlate._allBacteria ++ newTemporaryBacteria.filter(_.isDefined).map(_.get),
                            allFoodSources =
                              newPlate._allFoodSources,
                            plateStatistics =
                              modifiedPlateStatistics)
      newTemporaryBacteria
    }).flatten
    val liveBacteria = newBacteria.filter(_.isDefined).map(_.get)
    plate.copy(bacteria = liveBacteria,
               // We note that the viscosity of the agar increases as it solidifies due to evaporation of moisture,
               // and it settling down due to gravity.
               parameters = plate.parameters
                 .copy(mediumViscosity =
                         plate.viscosity + plate.viscosityIncrement),
               plateStatistics = plate.plateStatistics
                 .copy(locationHistory =
                         plate.locationHistoryUpdated,
                       _geneticallyUniqueBacteria =
                         liveBacteria.distinctBy(Bacterium.geneticDistinctionCriteria).length,
                       _stoppedBacteria =
                         liveBacteria.count(_.stopped)))
      .copy(foodSources = plate.foodSources.map(_.copy(environment = plate)))
  }

  def locationHistoryUpdated: LocationHistory = {
    var locationHistory =
      plateStatistics.locationHistory
    for (bacterium <- bacteria) {
      locationHistory.get(bacterium) match {
        case Some(points) => locationHistory = locationHistory.updated(bacterium, points :+ bacterium)
        case None => locationHistory += bacterium -> Seq(bacterium)
      }
    }
    locationHistory
  }

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
    for ((_, individualHistory) <- plateStatistics.locationHistory) {
      val segments =
        individualHistory
          .map(_.location)
          .elementsRepeated(2)
          .drop(1)
          .dropRight(1)
      val segmentColors = individualHistory.drop(1).map(_.visiblePaint)
      for ((((x1, y1), (x2, y2)), segmentColor) <- segments.pairWise.zip(segmentColors)) {
        canvas.setStroke(segmentColor)
        canvas.strokeLine(x1, y1, x2, y2)
      }
    }
    canvas.restore()
  }

  def drawBase(canvas: Canvas): Unit = {
    if ((canvas.getHeight min canvas.getWidth) < radius) {
      throw new IllegalArgumentException("Cannot draw plate in canvas smaller than plate")
    }
    val graphics = canvas.getGraphicsContext2D
    graphics.save()
    // Paint Glass-like background of plate
    graphics.setFill(visiblePaint)
    graphics.fillOval(safeLeftExtreme, safeTopExtreme,
                      innerDiameter, innerDiameter)
    graphics.restore()
    graphics.save()
  }

  def draw(canvas: Canvas): Unit = {
    drawBase(canvas)
    val graphics = canvas.getGraphicsContext2D
    //draw contents
    foodSources foreach (_ draw canvas)
    drawTrails(graphics)
    bacteria foreach (_ draw canvas)
    //draw wall of plate
    graphics.setStroke(Color.SILVER)
    graphics.setLineWidth(borderWidth)
    graphics.strokeOval(leftExtreme, topExtreme,
                        diameter, diameter)
    //Mask the central region so that we don't get bits and pieces outside the plate
    canvas.setClip(maskRegion)
    graphics.restore()
  }
}

object Plate {

  case class Parameters(mediumViscosity: Double = Defaults.viscosity,
                        mediumDensity: Double = Defaults.density,
                        mediumViscosityIncrement: Double = Defaults.viscosityIncrement,
                        borderFactor: Double = 0.01)

  private val random = ui.View.random
  type LocationHistory = Map[Bacterium, Seq[Bacterium]]

  case class PlateStatistics(_deadBacteria: Int,
                             _spawnedBacteria: Int,
                             _consumedFoodSources: Int,
                             _spawnedFoodSources: Int,
                             _geneticallyUniqueBacteria: Int,
                             _stoppedBacteria: Int,
                             locationHistory: LocationHistory) {
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

  object PlateStatistics {
    private val default = PlateStatistics(0, 0, 0, 0, 0, 0, Map())

    def apply(): PlateStatistics = default

    def onlySpawned(spawnedBacteria: Int, spawnedFoodSources: Int): PlateStatistics = {
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
  }

  def createBarePlate(width: Double, height: Double): Plate = {
    val plateCenter = (width / 2, height / 2)
    Plate((Defaults.xScale * width).toInt, (Defaults.yScale * height).toInt, plateCenter)
  }

  def createRandomizedPlate(width: Double, height: Double,
                            foodSourceSpawner: Plate => Seq[FoodSource],
                            bacteriaSpawner: Plate => Seq[Bacterium]): Plate =
    Plate.createBarePlate(width, height).initializedWith(foodSourceSpawner, bacteriaSpawner)

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

  def fromSerializableString(serializedString: String): Plate = ??? // TODO: Allow deserialization
}
