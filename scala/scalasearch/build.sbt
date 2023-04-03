val scala3Version = "3.2.2"

ThisBuild / scalaVersion := scala3Version
ThisBuild / organization := "xsearch"

// not sure the right syntax for this
// test / assembly := {}

lazy val scalaSearch = (project in file("."))
  .settings(
    name := "scalasearch",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "com.googlecode.json-simple" % "json-simple" % "1.1.1",
      "org.apache.commons" % "commons-compress" % "1.21",
      "org.scalactic" %% "scalactic" % "3.2.15",
      "org.scalatest" %% "scalatest" % "3.2.15" % Test,
      "com.novocode" % "junit-interface" % "0.11" % Test
    )


  )
