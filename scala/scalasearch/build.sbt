lazy val root = (project in file(".")).
  settings(
    name := "scalasearch",
    version := "1.0",
    scalaVersion := "2.9.2",
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-compress" % "1.8.1",
      "junit" % "junit" % "4.8.1" % "test",
      "org.scalatest" %% "scalatest" % "1.8" % "test"
    )
  )

