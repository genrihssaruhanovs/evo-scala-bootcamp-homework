name := "evo-scala-homework"

version := "0.2"

scalaVersion := "2.13.3"
val catsVersion = "2.2.0"
val catsScalacheckVersion = "0.2.0"
val log4CatsVersion = "1.1.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "io.chrisdavenport" %% "log4cats-slf4j" % log4CatsVersion,
  "io.chrisdavenport" %% "cats-scalacheck" % catsScalacheckVersion % Test,
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test)
