# big-visualizer
_The Big Data Visualizer for doing Big Analysis with the output of the Big Data Box._

This is a plotting program designed to work with large amounts of timeseries data (millions of points) and render it all at interactive rates to support exploratory data visualization.


# Developing Setup
This project is written in Scala, using the ScalaFX GUI toolkit (which is a thin wrapper around JavaFX)

## IntelliJ Setup
_If you plan to do significant development, setting up an IDE is highly recommended. We used IntelliJ Community Edition, which can be [downloaded here](https://www.jetbrains.com/idea/download)._

1. In IntelliJ, make sure the official (JetBrains) Scala plugin is installed.
2. To import the project, open the .sbt file.
3. Set up a run configuration to compile and run the program.
   - In IntelliJ, go to menu > Run > Edit Configurations...
   - Add a new Application configuration
   - For the module, select `big-visualizer`
   - For the main class, select `bigvis.Main`
4. If you want to run individual unit tests, you can open the unit test file, and click on the green arrow on the left hand side (next to the line number). This will also add a run configuration.
5. You can also set up a ScalaTest run configuration for all tests:
    - In IntelliJ, go to menu > Run > Edit Configurations...
    - Add a new ScalaTest configuration
    - For Test Kind, select All in Package
    - For Test Package, select `bigvis` 

## Command-line setup
_If you only need to build from source and do not plan on doing heavy development, you can just use command-line sbt, which can be [downloaded here](https://www.scala-sbt.org/download.html). sbt is a build tool for Scala-based projects._

1. Run `sbt run` in the repository root directory, to compile and run this program. sbt will automatically fetch any needed dependencies. 
2. Other useful sbt commands are:
   - `test`: runs all unit tests
   - `assembly`: compiles a JAR file 


# Background Tutorials and Resources
_Some resources if you're not familiar with Scala and/or JavaFX._

- Check out this [Scala tutorial](https://www.scala-exercises.org/scala_tutorial/terms_and_types). If you're familiar with imperative, object-oriented languages (like Java and C++), two new big concepts to focus on would be pattern matching and higher-order functions.
- If you haven't worked with desktop GUI toolkits (like Swing or wxWidgets), check out this [JavaFX tutorial and reference](http://tutorials.jenkov.com/javafx/index.html). We don't use FXML or CSS (so feel free to skip those parts), instead we use the [ScalaFX](http://www.scalafx.org/) thin wrapper.
