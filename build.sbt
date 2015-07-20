lazy val root = (project in file(".")).
  settings(
    name := "fcon",
    version := "0.1",
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq(
      "-feature",
      "-unchecked",
      "-deprecation"
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
      "org.scalatest" % "scalatest_2.11" % "2.2.4" % Test,
      "org.scalacheck" %% "scalacheck" % "1.12.4" % Test
    )
  )
