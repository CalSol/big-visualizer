name := "big-visualizer"

version := "0.1"

scalaVersion := "2.13.5"

scalacOptions += "-deprecation"

idePackagePrefix := Some("bigvis")

libraryDependencies ++= Seq(
  "org.scalafx" %% "scalafx" % "16.0.0-R25",
  "com.github.tototoshi" %% "scala-csv" % "1.3.8",

  "de.siegmar" % "fastcsv" % "2.1.0",

  "org.scalatest" %% "scalatest" % "3.2.0" % "test",
)

// JavaFX binary detection, from https://github.com/scalafx/ScalaFX-Tutorials/blob/master/hello-sbt/build.sbt
val javafxModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Mac") => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}
libraryDependencies ++= javafxModules.map(m => "org.openjfx" % s"javafx-$m" % "16" classifier osName)

// Force hardware rendering
// TODO does this actually do anything?
run / javaOptions += "-Dsun.java2d.opengl=true"
run / javaOptions += "-Dprism.order=es2"

assembly / mainClass := Some("bigvis.Main")

assembly / assemblyMergeStrategy := {
  case "module-info.class" => MergeStrategy.first
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}
