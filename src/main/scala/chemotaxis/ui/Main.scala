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
    val args = getParameters.getNamed.asScala
    val fps = args.get("fps").map(_.toDouble).getOrElse(20.0)
    val dynamicFrameTime = args.contains("dyn")
    val view = new View(stage, restart, fps, dynamicFrameTime)
    val (sceneBounds, rootPane) = view.initializeDisplay
    val scene = new javafx.scene.Scene(rootPane, sceneBounds.getWidth, sceneBounds.getHeight)
    // Command-line named arguments:
    // fps : Ideal/Desired FPS, default 20
    // fs : # food sources to be spawned, default ranged random
    // bs : # bacteria to be spawned, default ranged random
    // dyn : enable dynamic frame time, default false, switch
    //noinspection ZeroIndexToHead
    view.initializeState(args.get("fs").map(_.toInt), args.get("bs").map(_.toInt))
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