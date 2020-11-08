name := "scalasearch"

version := "0.1.0"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  "com.googlecode.json-simple" % "json-simple" % "1.1.1",
  "org.apache.commons" % "commons-compress" % "1.20",
  "org.scala-lang.modules" % "scala-xml_2.13" % "1.3.0",
  "org.scalatest" % "scalatest_2.13" % "3.2.2" % Test,
  "junit" % "junit" % "4.13.1" % Test
)

