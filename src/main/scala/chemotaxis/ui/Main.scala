package chemotaxis.ui

import javafx.application.{Application, Platform}
import javafx.scene.input.{KeyCode, KeyEvent}
import javafx.stage.{Screen, Stage}

import scala.collection.JavaConverters._
import scala.util.Try

/**
  * Main Class for testing
  */
class Main extends Application {
  def restart(stage: Stage): Unit = {
    cleanup(stage)
    startDisplay(stage)
  }

  override def start(primaryStage: Stage): Unit = restart(primaryStage)

  def cleanup(stage: Stage): Unit = {
    stage.hide()
    // Further cleanup unnecessary
  }

  def startDisplay(stage: Stage): Unit = {
    val primaryScreenBounds = Screen.getPrimary.getVisualBounds
    stage.setX(primaryScreenBounds.getMinX)
    stage.setY(primaryScreenBounds.getMinY)
    stage.setWidth(primaryScreenBounds.getWidth)
    stage.setHeight(primaryScreenBounds.getHeight)
    stage.setTitle("Chemotaxis simulation")
    stage.setFullScreenExitHint("")
    stage.setMaximized(true)
    val args = getParameters.getRaw.asScala
    val fps = if (args.length >= 3) args.lastOption.map(_.toDouble).getOrElse(20.0) else 20.0
    val view = new View(stage, restart, fps)
    val (sceneBounds, rootPane) = view.initializeDisplay
    val scene = new javafx.scene.Scene(rootPane, sceneBounds.getWidth, sceneBounds.getHeight)
    //noinspection ZeroIndexToHead
    view.initializeState(Try(args(0).toInt).toOption, Try(args(1).toInt).toOption)
    var animationTimer = view.simulationRunner(stage)

    def handleEventControllingAnimation(action: => Unit) {
      animationTimer.stop()
      action
      animationTimer.start()
    }

    var paused = false
    val fpsIncrement = 1
    scene.addEventHandler(KeyEvent.KEY_PRESSED, (event: KeyEvent) => event.getCode match {
      //@formatter:off
      case KeyCode.F5 =>
        animationTimer.stop()
        restart(stage)
      case KeyCode.F2 =>
        handleEventControllingAnimation { view.saveState() }
      case KeyCode.F3 =>
        handleEventControllingAnimation { animationTimer = view.restoreState(stage) }
      case KeyCode.S =>
        handleEventControllingAnimation { view.takeSnapshot(stage) }
      case KeyCode.ESCAPE =>
        animationTimer.stop()
        Platform.exit()
      case KeyCode.ENTER | KeyCode.SPACE =>
        if (paused) animationTimer.start() else animationTimer.stop()
        paused = !paused
      case KeyCode.BACK_SPACE =>
        handleEventControllingAnimation { if (view.frames > 1) view.goToFrame(view.frames - 1) }
      case KeyCode.EQUALS =>
        handleEventControllingAnimation { view.fps += fpsIncrement }
      case KeyCode.MINUS =>
        handleEventControllingAnimation { view.fps -= fpsIncrement }
      case KeyCode.F11 =>
        handleEventControllingAnimation { stage.setFullScreen(!stage.isFullScreen) }
      case _ => // Ignore
      // @formatter:on
    })
    stage.setScene(scene)
    stage.setResizable(false)
    stage.sizeToScene()
    animationTimer.start()
    stage.show()
  }
}

object Main {
  def main(args: Array[String]): Unit =
    Application.launch(classOf[Main], args: _*)
}