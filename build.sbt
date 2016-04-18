name := """typify"""

version := "1.0"

lazy val root = project.in(file("."))

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.0",
  "org.scalaz" %% "scalaz-core" % "7.2.2"
)

tutSettings
