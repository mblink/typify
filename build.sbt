Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala212 = "2.12.10"
lazy val scala213 = "2.13.1"

lazy val baseSettings = Seq(
  scalaVersion := scala212,
  crossScalaVersions := Seq(scala212/*, scala213*/),
  version := "3.0.0-LOCAL33",
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

lazy val cats = "org.typelevel" %% "cats-core" % "2.1.0"
lazy val circe = "io.circe" %% "circe-core" % "0.12.3"
lazy val playJson = "com.typesafe.play" %% "play-json" % "2.6.10"
lazy val shapeless = "com.chuusai" %% "shapeless" % "2.3.3"
lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.14.2" % "test"

lazy val typify = project.in(file("typify"))
  .settings(baseSettings)
  .settings(
    name := "typify",
    libraryDependencies ++= Seq(cats, shapeless, scalacheck),
    tutTargetDirectory := file("."),
    scalacOptions in Tut := (scalacOptions in (Compile, console)).value
  )
  .enablePlugins(TutPlugin)

lazy val playjsonTypify = project.in(file("play-json-typify"))
  .settings(baseSettings)
  .settings(
    name := "play-json-typify",
    libraryDependencies ++= Seq(cats, playJson, scalacheck)
  )
  .dependsOn(typify % "test->test;compile->compile")

lazy val circeTypify = project.in(file("circe-typify"))
  .settings(baseSettings)
  .settings(
    name := "circe-typify",
    libraryDependencies ++= Seq(circe, scalacheck),
  )
  .dependsOn(typify % "test->test;compile->compile")
