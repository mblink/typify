lazy val baseSettings = Seq(
  scalaVersion := "2.12.10",
  version := "3.0.0-LOCAL4",
  addCompilerPlugin("io.tryp" %% "splain" % "0.5.0" cross CrossVersion.patch),
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.patch),
  scalacOptions ++= Seq("-P:splain:all"),
  scalacOptions --= Seq(
    "-language:existentials",
    "-language:experimental.macros",
    "-language:implicitConversions"
  ),
  scalacOptions in (Compile, console) := scalacOptions.value.filterNot(x =>
    x.startsWith("-Ywarn-unused") || x.startsWith("-Xlint") || x.startsWith("-P:splain")),
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
  bintrayOrganization := Some("bondlink"),
  bintrayRepository := "Typify",
  bintrayReleaseOnPublish in ThisBuild := false
)

lazy val root = project.in(file("."))
  .aggregate(typify, playjsonTypify, circeTypify)
  .settings(
    publish := {},
    publishLocal := {},
    bintrayReleaseOnPublish in ThisBuild := false
  )

lazy val shapeless = "com.chuusai" %% "shapeless" % "2.3.3"
lazy val scalaz = "org.scalaz" %% "scalaz-core" % "7.2.26"
lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.14.2" % "test"

lazy val typify = project.in(file("typify"))
  .settings(baseSettings)
  .settings(
    name := "typify",
    libraryDependencies ++= Seq(shapeless, scalaz, scalacheck),
    tutTargetDirectory := file("."),
    scalacOptions in Tut := (scalacOptions in (Compile, console)).value
  )
  .enablePlugins(TutPlugin)

lazy val playjsonTypify = project.in(file("play-json-typify"))
  .settings(baseSettings)
  .settings(
    name := "play-json-typify",
    libraryDependencies ++= Seq(scalaz, scalacheck, "com.typesafe.play" %% "play-json" % "2.6.10")
  )
  .dependsOn(typify % "test->test;compile->compile")

lazy val circeTypify = project.in(file("circe-typify"))
  .settings(baseSettings)
  .settings(
    name := "circe-typify",
    libraryDependencies ++= Seq(scalaz, scalacheck, "io.circe" %% "circe-core" % "0.11.1"),
  )
  .dependsOn(typify % "test->test;compile->compile")
