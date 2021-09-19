name := "big-visualizer"

version := "0.1"

scalaVersion := "2.13.5"

idePackagePrefix := Some("bigvis")

libraryDependencies ++= Seq(
  "org.scalafx" %% "scalafx" % "15.0.1-R21",
  "com.github.tototoshi" %% "scala-csv" % "1.3.8",

  "org.scalatest" %% "scalatest" % "3.2.0" % "test",
)

// JavaFX binary detection, from http://www.scalafx.org/docs/quickstart/
lazy val javaFXModules = {
  // Determine OS version of JavaFX binaries
  lazy val osName = System.getProperty("os.name") match {
    case n if n.startsWith("Linux")   => "linux"
    case n if n.startsWith("Mac")     => "mac"
    case n if n.startsWith("Windows") => "win"
    case _                            =>
      throw new Exception("Unknown platform!")
  }
  // Create dependencies for JavaFX modules
  Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
      .map( m=> "org.openjfx" % s"javafx-$m" % "15.0.1" classifier osName)
}

libraryDependencies ++= javaFXModules

// Need to increase JVM memory sizes because of the bigness of the data
run / javaOptions ++= Seq(
  "-Xms1G", "-Xmx8G")

// Force hardware rendering
// TODO does this actually do anything?
run / javaOptions += "-Dsun.java2d.opengl=true"
run / javaOptions += "-Dprism.order=es2"
