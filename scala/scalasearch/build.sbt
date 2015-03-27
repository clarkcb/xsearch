lazy val root = (project in file(".")).
  settings(
    name := "scalasearch",
    version := "1.0",
    scalaVersion := "2.11.4",
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-compress" % "1.8.1",
      "junit" % "junit" % "4.12" % "test",
      "org.scala-lang.modules" %% "scala-xml" % "1.0.3",
      "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
    )
  )

