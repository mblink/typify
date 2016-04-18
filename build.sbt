name := """shapeless-tut"""

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.0",
  "org.scalaz" %% "scalaz-core" % "7.2.2",
  "org.json4s" %% "json4s-jackson" % "3.3.0"
)

tutSettings
