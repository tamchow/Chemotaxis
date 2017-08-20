package chemotaxis.extensions

import javafx.geometry.{Bounds, Dimension2D, Point2D}
import javafx.scene._
import canvas.Canvas
import paint.Color
import shape.{Circle, Line, Rectangle}

import chemotaxis.geometry.Collisions

import scala.annotation.tailrec
import scala.language.implicitConversions

/**
  * Some useful implicit extensions to some library classes
  */

object Extensions {

  final implicit class RicherString(base: String) {
    def substringFromFirst(start: String, exclusive: Boolean = false): String =
      base.substring(base.indexOf(start) + (if (exclusive) 1 else 0))

    def substringFromLast(start: String, exclusive: Boolean = false): String =
      base.substring(base.lastIndexOf(start) + (if (exclusive) 1 else 0))

    def substringToFirst(end: String, inclusive: Boolean = false): String =
      base.substring(0, base.indexOf(end) + (if (inclusive) 1 else 0))

    def substringToLast(end: String, inclusive: Boolean = false): String =
      base.substring(0, base.lastIndexOf(end) + (if (inclusive) 1 else 0))
  }

  final implicit class RichRandom[T <: java.util.Random](val base: T) extends AnyVal {
    def nextBoolean(probability: Double): Boolean = nextDouble < probability

    def nextDouble: Double = base.nextDouble()

    def nextDoublesForProbability(n: Int): Seq[Double] = nextDoublesSummingTo(1.0, n)

    /**
      * @usecase Generating numbers which'll add up to produce probabilities and the like.
      * @param sum the reuired sum of all the random numbers
      * @param n   the number of random numbers required in this list
      * @return a [[scala.Seq]] of `n` random numbers,
      *         each less than or equal to `sum`
      *         with sum equal to `sum`,
      *         sorted in descending order
      */
    def nextDoublesSummingTo(sum: Double, n: Int): Seq[Double] =
      if (n < 0)
        throw new IllegalArgumentException(s"The number of values to produce must be >= 0; it is $n")
      else if (n == 0) Seq()
      else {
        val randomNumbers =
          ((1 until n).view
             .map(_ =>
                    nextDouble(0, sum))
           ++ Seq(0.0, sum)).sorted
        (randomNumbers.init.zip(randomNumbers.tail) map {
          case (a, b) => b - a
        }).sortWith(_ > _) // Sort in descending order
      }

    def nextDouble(max: Double): Double =
      nextDouble(0.0, max)

    def nextDouble(min: Double, max: Double): Double =
      nextDouble * Math.abs(max - min) + (if (min < max) min else max)

    def nextDouble(includeZero: Boolean, includeOne: Boolean): Double = {
      var d = 0.0
      do {
        // grab a value, initially from half-open [0.0, 1.0)
        d = nextDouble
        // if includeOne, with 1/2 probability, push to [1.0, 2.0)
        if (includeOne && base.nextBoolean)
          d += 1.0
      }
      while (
      // everything above 1.0 is always invalid
        (d > 1.0) ||
        // if we're not including zero, 0.0 is invalid
        (!includeZero && d == 0.0))
      d
    }

  }

  final implicit class RichSeq[+T](base: Seq[T]) {
    lazy val withIndices: Seq[(T, Int)] = base.zipWithIndex

    def nullsRemoved: Seq[T] =
      removed(null)

    def removed[B >: T](item: B): Seq[T] =
      removed(item == _)

    def removed(itemSelector: T => Boolean): Seq[T] =
      base.filter(!itemSelector(_))

    def removedAtIndex(index: Int): Seq[T] =
      withIndices.removed((itemInfo: (T, Int)) => index == itemInfo._2).unzip._1

    def findWithIndex(predicate: T => Boolean): Option[(T, Int)] =
      withIndices.find { case (item, _) => predicate(item) }

    def elementsRepeated(n: Int): Seq[T] =
      base.flatMap(element => Seq.fill(n)(element))

    def pairWise: Seq[(T, T)] =
      base.grouped(2).map { case Seq(first, second) => (first, second) }.toSeq

    def distinctBy[S](distinction: T => S): Seq[T] = {
      var seen = Set[S]()
      for (item <- base if !seen.contains(distinction(item))) yield {
        seen += distinction(item)
        item
      }
    }
  }

  type Vector2D = (Double, Double)
  type Point = Vector2D

  final implicit class RichVector2D(base: Vector2D) {
    lazy val (x, y) = (base._1, base._2)

    def +(that: Vector2D): Vector2D = (x + that.x, y + that.y)

    lazy val unary_- : Vector2D = -1.0 * base

    def -(that: Vector2D): Vector2D = base + (-that)

    lazy val normSquared: Double = x.squared + y.squared

    lazy val norm: Double = math.hypot(x, y)

    def cross(that: Vector2D): Double =
      x * that.y - y * that.x

    def dot(that: Vector2D): Double =
      x * that.x + y * that.y

    def *(that: Vector2D): Double = dot(that)

    def X(that: Vector2D): Double = cross(that)

    def distance(that: Vector2D): Double =
      (that.x - x, that.y - y).norm

    lazy val angle: Double = Math.atan2(y, x)

    def projectionOf(that: Vector2D): Vector2D =
      ((that * base) / base.normSquared) * base

    def unary_~ : Double = norm

    private def lerp(from: Double, to: Double, bias: Double): Double =
      to + (from - to) * bias

    def lerp(to: Vector2D, bias: Double): Vector2D =
      (lerp(x, to.x, bias), lerp(y, to.y, bias))

    lazy val rounded: Vector2D = (x.round, y.round)

    lazy val roundedDown: Vector2D = (x.toInt, y.toInt)

    lazy val toPoint2D: Point2D = new Point2D(base.x, base.y)
  }

  object Vector2D {
    def fromMagnitudeAndAngle(magnitude: Double, angle: Double): Vector2D =
      magnitude * (Math.cos(angle), Math.sin(angle))
  }

  case class Interval(start: Double, startOpen: Boolean,
                      end: Double, endOpen: Boolean) {

    override def toString: String =
      s"${getClass.getSimpleName}" +
      s"{${if (startOpen) "(" else "["}$start," +
      s" $end${if (endOpen) ")" else "]"}}"

    def contains(x: Double, tolerance: Double = 0.0): Boolean =
      (if (startOpen) (x - start) > tolerance else (x - start) >= tolerance) &&
      (if (endOpen) (x - end) < tolerance else (x - end) <= tolerance)

    def containsExact(x: Double): Boolean =
      (if (startOpen) x > start else x >= start) &&
      (if (endOpen) x < end else x <= end)
  }

  object Interval {

    import MathUtilities.Epsilon

    def neighbourhoodOf(x: Double): Interval =
      Interval(x - Epsilon, startOpen = false, x + Epsilon, endOpen = false)
  }

  final implicit class RicherDouble(val base: Double)
    extends AnyVal {

    def ^(exp: Int): Double = {
      @tailrec
      def pow(exp: Int, accumulator: Double = base): Double =
        exp match {
          case 0 => 1.0
          case 1 => accumulator
          case 2 => accumulator * base
          case _ => pow(exp - 1, accumulator * base)
        }

      pow(exp)
    }

    @inline def ^(exp: Double): Double =
      Math.pow(base, exp)

    // Specialization for performance reasons
    @inline def squared: Double =
    base * base

    @inline def *(that: Vector2D): Vector2D =
      (base * that.x, base * that.y)

    @inline def fractional: Double = {
      val absBase = base.abs
      absBase - absBase.toLong
    }

    @inline def in(interval: Interval, tolerance: Double = MathUtilities.Epsilon): Boolean =
      interval.contains(base, tolerance)

    @inline def inExact(interval: Interval): Boolean =
      interval.containsExact(base)
  }

  implicit def boundsToRectangle(bounds: Bounds): Rectangle =
    new Rectangle(bounds.getMinX, bounds.getMinY,
                  bounds.getWidth, bounds.getHeight)

  final implicit class RichCanvas(val canvas: Canvas) {
    def size: Dimension2D =
      new Dimension2D(canvas.getWidth, canvas.getHeight)
  }

  final implicit class RichLine(base: Line) {
    val (startX, startY, endX, endY) =
      (base.getStartX, base.getStartY,
        base.getEndX, base.getEndY)
    lazy val slope: Double = {
      val dx = endX - startX
      if (Math.abs(dx) < MathUtilities.Epsilon) Double.NaN else (endY - startY) / dx
    }

    lazy val intercept: Double = {
      val m = slope
      if (m.isNaN) Double.PositiveInfinity else startY - (m * startX)
    }

    def lineEpsilon(threshold: Double = MathUtilities.ScreenEpsilon): Double =
      distance * threshold

    lazy val startAndEndPoints:
      ((Double, Double), (Double, Double)) =
      ((startX, startY), (endX, endY))

    lazy val distance: Double = {
      val (lineStart, lineEnd) = startAndEndPoints
      lineStart distance lineEnd
    }

    def contains(point: Point): Boolean = {
      val (lineStart, lineEnd) = startAndEndPoints
      Math.abs(((lineStart distance point) + (point distance lineEnd)) - distance) < lineEpsilon()
    }
  }

  object RichLine {
    def apply(start: Point, end: Point): Line =
      new Line(start.x, start.y, end.x, end.y)

    def unapply(line: Line):
    Option[(Double, Double, Double, Double)] =
      line match {
        case null => None
        case _ => Some((line.getStartX, line.getStartY,
                         line.getEndX, line.getEndY))
      }
  }

  object BuilderStyle {
    def build[T](node: T)(initializer: T => Unit): T = {
      initializer(node)
      node
    }
  }

}

object ColorUtilities {
  val defaultTolerance: Double = 15.0

  @inline def randomColor: Color = Color.hsb(math.random * 360, 1.0, 1.0)

  @inline def hueDifference(color1: Color, color2: Color): Double = (color2.getHue - color1.getHue).abs

  @inline def perceptuallyDifferent(color1: Color, color2: Color, tolerance: Double = defaultTolerance): Boolean =
    !perceptuallySimilar(color1, color2, tolerance)

  @inline def perceptuallySimilar(color1: Color, color2: Color, tolerance: Double = defaultTolerance): Boolean =
    hueDifference(color1, color2) < tolerance
}

object MathUtilities {

  import Extensions._

  @inline val Sqrt2: Double = Math.sqrt(2)
  @inline val Epsilon: Double = 1E-6
  @inline val ScreenEpsilon: Double = 1E-3
  @inline val Pi = Math.PI
  @inline val PiBy2: Double = Pi / 2.0
  @inline val PiBy4: Double = Pi / 4.0
  @inline val PiTimes2: Double = 2.0 * Pi

  val normalInterval: Interval = Interval(0, startOpen = true, 1, endOpen = true)
  val normalIntervalClosed: Interval = Interval(0, startOpen = false, 1, endOpen = false)
  val nbdOf1: Interval = Interval.neighbourhoodOf(1.0)

  sealed trait Limits[T] {
    val MaxValue: T
    val MinValue: T
  }

  implicit object IntLimits extends Limits[Int] {
    val MaxValue: Int = Int.MaxValue
    val MinValue: Int = Int.MinValue
  }

  implicit object DoubleLimits extends Limits[Double] {
    val MaxValue: Double = Double.MaxValue
    val MinValue: Double = Double.MinValue
  }

  @inline def verify(valueToCheck: => Double, default: => Double): Double =
    if (valueToCheck.isNaN || valueToCheck.isInfinite) default else valueToCheck

  @inline def clamp[T](min: T, max: T)(value: T)
                      (implicit toOrdered: T => Ordered[T]): T =
    if (value < min) min
    else if (value > max) max
    else value

  @inline def clampAbove[T: Numeric : Limits](min: T): T => T =
    clamp[T](min, implicitly[Limits[T]].MaxValue)

  @inline def clampNonNegative[T: Numeric : Limits](value: T): T =
    clampAbove[T](implicitly[Numeric[T]].zero).apply(value)

  @inline def clampNatural[T: Numeric : Limits](value: T): T =
    clampAbove[T](implicitly[Numeric[T]].one).apply(value)

  @inline def clampNormalized:
  Double => Double =
    clamp(0.0, 1.0)

  def logisticFunction(x0: Double, C: Double, L: Double, K: Double):
  Double => Double =
    x => L / (C + Math.exp(-K * (x - x0)))

  val normalizedLogisticFunction:
    Double => Double =
    x => logisticFunction(0.5, 1, 1, 9)(clampNormalized(x))

  val coNormalizedLogisticFunction:
    Double => Double =
    x => 1 - normalizedLogisticFunction(x)

  private val RootPiBy2 = math.sqrt(Pi) / 2.0

  /**
    * Approximates the normal error function by the Bürmann series
    *
    * @param x a real number
    * @return erf(x)
    */
  def erf(x: Double): Double =
    (1.0 / RootPiBy2) * math.signum(x) * math.sqrt(1 - math.exp(-x.squared)) *
    (RootPiBy2 + (31.0 / 200.0) * math.exp(-x.squared) + (341.0 / 8000.0) * math.exp(-2 * x.squared))

  def gaussianProbability(mean: Double, deviation: Double, scaleFactor: Double = 2.0 * math.sqrt(2.0)): Double => Double =
    x => erf(scaleFactor * ((x - mean) / (math.sqrt(2) * deviation))).abs.min(1.0)

  object Geometry {

    def solveQuadratic(a: Double, b: Double, c: Double):
    Option[Seq[Double]] =
      discriminant(a, b, c) match {
        case imaginary if imaginary < 0 => None
        case real =>
          val (n_b, intermediate, _2a) = (-b, Math.sqrt(real), 2 * a)
          Some(Seq((n_b - intermediate) / _2a, (n_b + intermediate) / _2a))
      }

    def discriminant(a: Double, b: Double, c: Double): Double =
      b.squared - (4 * a * c)

    /*
    * About `extensionScale`:
    * 0.0: drop on circle boundary - tends to get some bacteria caught into tumble loops at high frame rate for low speed
    * (they can't move away enough from the boundary in a frame). Lowering the frame rate or increasing bacterial speed fixes this.
    * 0.5: tumble loops less prevalent
    * 1.0: drop at about the incoming distance on the chord
    */
    def lineIntersectsCircle(line: Line, circle: Circle, extensionScale: Double = 1.0):
    Option[Point] = {
      val (lineStart, lineEnd) = line.startAndEndPoints
      val _extensionScale = clampNormalized(extensionScale)
      val screenPoints = (points: Option[Seq[Double]], eqn: (Double) => Point) =>
        points.map(_.map(eqn)
                     .filter(point => (point distance lineStart) >= line.lineEpsilon())
                     .map(intersection => intersection.lerp(lineStart,
                                                            (line.distance / RichLine(lineStart, intersection).distance).fractional * _extensionScale))
                     .sortBy(_ distance lineStart))
          .flatMap(points => if (_extensionScale < 0.5) points.lastOption else points.headOption)
      if (Collisions.circleContainsPoint(circle, lineStart) &&
          Collisions.circleContainsPoint(circle, lineEnd)) None else {
        val (m, c) = (line.slope, line.intercept)
        val (p, q, r) = (circle.getCenterX, circle.getCenterY, circle.getRadius)
        if (m.isNaN) {
          val Cx = line.startX
          screenPoints(solveQuadratic(1, -2 * q,
                                      (Cx - p).squared + q.squared - r.squared),
                       y => (Cx, y).roundedDown)
        } else {
          screenPoints(solveQuadratic(1 + m.squared, 2 * (((m * c) - (m * q)) - p),
                                      q.squared - r.squared + p.squared - (2 * c * q) + c.squared),
                       x => (x, m * x + c).roundedDown)
        }
      }
    }
  }

}