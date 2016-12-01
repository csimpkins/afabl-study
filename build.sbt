lazy val commonSettings = Seq(
  version := "0.3.0",
  scalaVersion := "2.11.8",
  crossScalaVersions := Seq("2.11.7"),
  organization := "org.afabl"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "afabl-study",
    resolvers += "AFABL Repo" at "http://repo.afabl.org/",
    libraryDependencies ++= Seq(
      "org.afabl" %% "afabl" % "0.3.0",
      "org.scalactic" %% "scalactic" % "3.0.0",
      "org.scalatest" %% "scalatest" % "3.0.0" % "test",
      "ch.qos.logback" %  "logback-classic" % "1.1.7",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.4.0"
    )
  )
