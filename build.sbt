ThisBuild / scalaVersion     := "3.1.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.pc_parser"
ThisBuild / organizationName := "pc_parser"

lazy val root = project.in(file("."))
  .settings(
    name := "pc_parser",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
      "org.scalatest" %% "scalatest" % "3.2.10" % "test",
    ),
  )
