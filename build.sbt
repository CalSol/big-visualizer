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

  "me.shadaj" %% "scalapy-core" % "0.5.0",
)
//adds Python native libraries and configures SBT to run in a new JVM instance (for ScalaPy)
fork := true

import scala.sys.process._
lazy val pythonLdFlags = {
  val withoutEmbed = "python3-config --ldflags".!!
  if (withoutEmbed.contains("-lpython")) {
    withoutEmbed.split(' ').map(_.trim).filter(_.nonEmpty).toSeq
  } else {
    val withEmbed = "python3-config --ldflags --embed".!!
    withEmbed.split(' ').map(_.trim).filter(_.nonEmpty).toSeq
  }
}

lazy val pythonLibsDir = {
  pythonLdFlags.find(_.startsWith("-L")).get.drop("-L".length)
}

javaOptions += s"-Djna.library.path=$pythonLibsDir"
javaOptions in Test += s"-Djna.library.path=$pythonLibsDir"
// JavaFX binary detection, from https://github.com/scalafx/ScalaFX-Tutorials/blob/master/hello-sbt/build.sbt
val javafxModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Mac") => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}
libraryDependencies ++= javafxModules.map(m => "org.openjfx" % s"javafx-$m" % "16" classifier osName)

// Need to increase JVM memory sizes because of the bigness of the data
run / javaOptions ++= Seq(
  "-Xms1G", "-Xmx8G")

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
