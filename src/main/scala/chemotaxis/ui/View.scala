package chemotaxis.ui

import java.nio.file.{Files, Path, Paths}
import java.util
import javafx.animation.AnimationTimer
import javafx.application.Platform
import javafx.embed.swing.SwingFXUtils
import javafx.event.ActionEvent
import javafx.geometry.{Dimension2D, HPos, Insets, Pos}
import javafx.scene._
import javafx.scene.canvas.Canvas
import javafx.scene.control._
import javafx.scene.effect.BlendMode
import javafx.scene.input.MouseEvent
import javafx.scene.layout._
import javafx.scene.paint.Color
import javafx.scene.text.Font
import javafx.stage.{FileChooser, Stage}
import javax.imageio.ImageIO

import chemotaxis.biology.{Bacterium, FoodSource, Environment}
import chemotaxis.extensions.Extensions.{BuilderStyle, RicherString}
import chemotaxis.extensions.MathUtilities._

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

/**
  * Encapsulates the UI of the simulation
  */
class View(providedStage: Stage, restartHandler: Stage => Unit, providedFPS: Double) {

  import View._

  private val rootPane = new GridPane
  private val statusLabel = new Label()
  private val indexInput = new TextField()
  private val changeIndexButton = new Button(indexerButtonText)
  private val stage: Stage = providedStage
  private val sceneSize = new Dimension2D(stage.getWidth, stage.getHeight)
  private val canvas = new Canvas(sceneSize.getWidth, (sceneSize.getHeight * canvasSizeFraction).toInt)
  private var current: Environment = Environment.createBare(canvas.getWidth, canvas.getHeight)
  private var _fps: Double = clampNonNegative(providedFPS)
  private var _frames = 0

  def fps: Double = _fps

  def fps_=(fps: Double): Unit = {
    _fps = clampNonNegative(fps)
  }

  def frames: Int = _frames

  /**
    * We use a rewind-overwrites-history approach here instead of something like timeline branching
    * because branching becomes prohibitively CPU-and-memory intensive in conjunction with the rewind functionality implemented in [[Main]]
    */
  private val history = new ArrayBuffer[Environment](estimatedMaxFrames)

  private val historyIndex = newIdentityHashSet[Environment]()

  /**
    * @param foodSourcesSpawnCount The number of food sources to spawn, if available
    * @param bacteriaSpawnCount    The number of bacteria to spawn, if available
    */
  def initializeState(foodSourcesSpawnCount: Option[Int] = None, bacteriaSpawnCount: Option[Int] = None): Unit = {
    current = current.initializedWith(FoodSource.spawnRandomFoodSources(foodSourcesSpawnCount.getOrElse(rng.nextInt(5) + (if (rng.nextBoolean()) 1 else 0)), _),
                                      Bacterium.spawnRandomBacteria(bacteriaSpawnCount.getOrElse(rng.nextInt(50) + 1), _))
  }

  def initializeDisplay: (Dimension2D, GridPane) = {
    rootPane.setPrefSize(sceneSize.getWidth, sceneSize.getHeight)
    val rootConstraintsTop = new RowConstraints
    rootConstraintsTop.setPercentHeight(maxPercentage * canvasSizeFraction)
    rootConstraintsTop.setVgrow(Priority.NEVER)
    val rootConstraintsBottom = new RowConstraints
    rootConstraintsBottom.setPercentHeight(maxPercentage - rootConstraintsTop.getPercentHeight)
    rootConstraintsBottom.setVgrow(Priority.ALWAYS)
    rootPane.getRowConstraints.addAll(rootConstraintsTop, rootConstraintsBottom)
    rootPane.setVgap(sceneSize.getHeight * 0.01)
    val canvasBox = new VBox()
    canvasBox.setFillWidth(true)
    rootPane.setStyle(/*Stainless Steel*/ "-fx-background:#d8d8d8;")
    canvas.setBlendMode(BlendMode.SRC_ATOP)
    canvasBox.getChildren.add(canvas)
    statusLabel.setFont(font)
    val statusBox = new VBox()
    statusBox.setPadding(new Insets(0, 0, 0, sceneSize.getWidth * 0.02))
    statusBox.getChildren.add(statusLabel)
    indexInput.setFont(font)
    indexInput.setPrefColumnCount(indexInputPrompt.length)
    indexInput.setMaxWidth(indexInputPrompt.length * fontSize)
    indexInput.setFocusTraversable(false)
    indexInput.setPromptText(indexInputPrompt)
    val infoPane = new GridPane
    val infoPaneColumnConstraints = new ColumnConstraints()
    infoPaneColumnConstraints.setHgrow(Priority.NEVER)
    infoPaneColumnConstraints.setPercentWidth(midPercentage)
    infoPaneColumnConstraints.setHalignment(HPos.LEFT)
    infoPane.getColumnConstraints.addAll(infoPaneColumnConstraints, infoPaneColumnConstraints)
    val indexBox = new VBox(sceneSize.getHeight * 0.02)
    changeIndexButton.setFocusTraversable(false)
    indexBox.setAlignment(Pos.CENTER)
    indexBox.getChildren.addAll(indexInput, changeIndexButton)
    infoPane.add(statusBox, 0, 0)
    infoPane.add(indexBox, 1, 0)
    infoPane.setHgap(sceneSize.getWidth * 0.05)
    rootPane.add(canvasBox, 0, 0)
    rootPane.add(infoPane, 0, 1)
    (sceneSize, rootPane)
  }

  def goToFrame(index: Int): Unit = {
    if (index < 1 || index > history.length)
      throw new IllegalArgumentException(s"Index $index out of bounds, should be in [1, ${
        history.length
      }]")
    val directIndex = index - 1
    current = history(directIndex)
    if (frames == history.length)
      history.remove(directIndex)
    _frames = index
  }

  def saveState(): Unit = {

    import java.time._

    new Thread(() => {
      val sessionSaveFileName = LocalDateTime.now.format((new format.DateTimeFormatterBuilder)
                                                           .appendPattern("HH-mm-ss_dd-LL-yyyy_")
                                                           .toFormatter) + baseSessionDBName
      Files.write(cwdPath.resolve(sessionSaveFileName), toSerializableString.asJava)
    }).start()
  }

  def toSerializableString: Seq[String] = ???

  def restoreState(stage: Stage): AnimationTimer = {
    val restoreFile = showFileChooserDialog("Choose the session database file to restore from",
                                            createExtensionFilterList(sessionDatabaseExtensionInfo))(stage, baseSessionDBName)
    val data = Files.readAllLines(restoreFile).asScala
    fps = data(1).toDouble
    historyIndex.clear()
    history.clear()
    history.sizeHint(data.length - 2)
    history ++ data.drop(2).map(Environment.fromSerializableString)
    historyIndex.sizeHint(history.length)
    historyIndex ++= history
    _frames = clamp(0, history.length)(data.head.toInt)
    simulationRunner(stage)
  }

  def simulationRunner(stage: Stage): AnimationTimer = {
    _frames = 0
    val animationTimer = new AnimationTimer() {
      //noinspection ScalaUnusedSymbol
      private var (deadDialogShowing_?, stoppedDialogShowing_?) = (false, false)
      private var lastUpdate = 0L

      override def handle(now: Long): Unit = {
        val frameTime = 1.0 / fps
        if (now - lastUpdate >= (frameTime * 1E9).toLong) {
          canvas.getGraphicsContext2D.clearRect(0, 0, stage.getWidth, stage.getHeight)
          val plateStatistics = current.statistics

          import plateStatistics._

          val fpsText = s"FPS: (real = " +
                        s"${"%.2f".format(1E9 / (now - lastUpdate))}, " +
                        s"ideal = $fps), " +
                        s"frame $frames (of ${history.length} total)"
          val spawnText = s"$spawnedBacteria " +
                          s"bacteri${if (spawnedBacteria != 1) "a" else "um"} & " +
                          s"$spawnedFoodSources food source${if (spawnedFoodSources != 1) "s" else ""} were spawned"
          statusLabel.setText(spawnText +
                              s", $fpsText\n$deadBacteria " +
                              s"bacteri${if (deadBacteria != 1) "a have" else "um has"} died" +
                              s", ${if (allFoodSourcesConsumed_?) "all" else s"$consumedFoodSources"} " +
                              s"food source${if (allFoodSourcesConsumed_? || consumedFoodSources != 1) "s have" else " has"} been consumed." +
                              s"\nGenetically Unique Bacteria = $geneticallyUniqueBacteria, Max Living Generation = $maxGeneration.")
          current draw canvas
          if (!historyIndex(current)) {
            if (history.length <= frames)
              history.append(current)
            else
              history(frames) = current
            historyIndex += current
          }
          // Don't pause the animation, continue updating it while the notification dialog is showing.
          //noinspection ScalaUnnecessaryParentheses
          if (allBacteriaDead_? &&
              (!(deadDialogShowing_? /*|| stoppedDialogShowing_?*/))) {
            Platform.runLater(() => showDialog("All bacteria have died. Restart simulation?"))
            deadDialogShowing_? = true
          }
          // Not extremely useful - just wait for the stopped bacteria to die.
          //            else if (allBacteriaStoppedButNotDead_? &&
          //                     (!(deadDialogShowing_? || stoppedDialogShowing_?))) {
          //            Platform.runLater(() => showDialog("All bacteria have stopped. Restart simulation?"))
          //            stoppedDialogShowing_? = true
          //          }
          else {
            current = current.updated(frameTime).selfConsistent
            lastUpdate = now
          }
          _frames += 1
        }
      }
    }
    changeIndexButton.addEventHandler(ActionEvent.ACTION, (_: ActionEvent) => {
      canvas.requestFocus()
      val text = indexInput.getText.trim
      if (!text.isEmpty) {
        goToFrame(text.toInt)
        animationTimer.handle(System.nanoTime())
      }
    })
    canvas.addEventHandler(MouseEvent.ANY, (event: MouseEvent) =>
      if (event.getPickResult.toString.contains("Canvas")) {
        current.react(event) match {
          case Some(newPlate) => current = newPlate.selfConsistent
          case None => ()
        }
      })
    animationTimer
  }

  def takeSnapshot(stage: Stage): Unit = {
    val canvasSnapshot = rootPane.snapshot(snapshotParameters, null)
    val path = showFileChooserDialog("Choose file to save snapshot to",
                                     createExtensionFilterList(imageExtensionInfo))(stage, fileName)
    new Thread(() => {
      val rawFileName = path.toFile.getName
      ImageIO.write(SwingFXUtils.fromFXImage(canvasSnapshot, null),
                    rawFileName.substringFromLast(".", exclusive = true),
                    path.toFile)
    }).start()
  }

  private def createExtensionFilterList(extensionFilterInfo: Seq[(String, String)]): Seq[FileChooser.ExtensionFilter] =
    extensionFilterInfo.map {
                              case (name, extension) => new FileChooser.ExtensionFilter(name, extension)
                            }

  import BuilderStyle._

  private def showFileChooserDialog(message: String, extensionFilters: Seq[FileChooser.ExtensionFilter])
                                   (stage: Stage, initialFileName: String): Path = {
    //@formatter:off
    val fileChooser = build(new FileChooser) { temp => {
      temp.setTitle(message)
      temp.setInitialDirectory(cwdPath.toFile)
      temp.setInitialFileName(initialFileName)
      temp.getExtensionFilters.addAll(extensionFilters: _*)
    }}
    //@formatter:on
    Option(Paths.get(fileChooser.showSaveDialog(stage).toURI))
      .getOrElse(cwdPath.resolve(initialFileName))
  }

  private def showDialog(text: String): Unit = {
    //@formatter:off
    val dialog = build(new Dialog[ButtonType]) { temp => {
      temp.getDialogPane.getButtonTypes.addAll(ButtonType.CLOSE, ButtonType.OK, ButtonType.CANCEL)
      temp.setTitle("Simulation Paused")
      temp.setContentText(text)
    }}
    dialog.showAndWait()
      .ifPresent { case ButtonType.CLOSE =>
                     Platform.exit()
                   case ButtonType.OK =>
                     restartHandler(stage)
                   case ButtonType.CANCEL => ()
                   case other =>
                     throw new UnsupportedOperationException(s"$other")
                 }
    //@formatter:on
  }
}

object View {
  val loggingEnabled: Boolean = true

  private val fontSize = 18
  private lazy val font = Font.font(fontSize)
  private val (indexInputPrompt, indexerButtonText) = ("Go to Frame Number", "Go to")
  private lazy val cwdPath = Paths.get("").toAbsolutePath.normalize()
  private val fileName = "snapshot.png"
  private val baseSessionDBName = "session.db"
  private lazy val imageExtensionInfo =
    Seq("All image files" -> "*.*",
        "PNG" -> ".png")

  private lazy val sessionDatabaseExtensionInfo =
    Seq("Database files" -> ".db")

  private val estimatedMaxFrames = 10000

  private val (maxPercentage, midPercentage, canvasSizeFraction) = (100, 50, 0.9)

  private def newIdentityHashSet[E](): collection.mutable.Set[E] =
    util.Collections.newSetFromMap(new util.IdentityHashMap[E, java.lang.Boolean]()).asScala

  def log(message: String): Unit = println(message)

  //noinspection TypeAnnotation
  lazy val rng = new java.security.SecureRandom()

  lazy val snapshotParameters: SnapshotParameters =
  //@formatter:off
    BuilderStyle.build(new SnapshotParameters) { _.setFill(Color.TRANSPARENT) }
  //@formatter:on
}