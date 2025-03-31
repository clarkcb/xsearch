val scala3Version = "3.6.2"

ThisBuild / scalaVersion := scala3Version
ThisBuild / organization := "xsearch"

lazy val scalaSearch = (project in file("."))
  .settings(
    name := "scalasearch",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.json" % "json" % "20250107",
      "org.apache.commons" % "commons-compress" % "1.27.1",
      "org.scalactic" %% "scalactic" % "3.2.19",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
      "com.github.sbt" % "junit-interface" % "0.13.3" % Test
    )
  )

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _                        => MergeStrategy.first
}
