name := "ChemotaxisSimulator"

version := "0.8"
organization := "in.github.tamchow"
scalaVersion := "2.12.2"

scalacOptions += "-feature"

jfxSettings

JFX.mainClass := Some("chemotaxis.ui.Main")

mainClass in assembly := Some("chemotaxis.ui.Main")
assemblyJarName in assembly := "chemotaxis.jar"