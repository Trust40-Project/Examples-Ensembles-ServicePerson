name := "trust4.0-ensembles"

version := "1.0"

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(

  // Required for mpmens
  "org.scala-lang" % "scala-reflect" % "2.12.4",
  "org.choco-solver" % "choco-solver" % "4.0.0",

  // Required for map2d trait
  "de.ummels" %% "scala-prioritymap" % "1.0.0",

  // Required for statespace and statistics traits
  "org.apache.commons" % "commons-math3" % "3.6.1"

  // Unit tests
  // "org.scalactic" %% "scalactic" % "3.0.1",
  // "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)
