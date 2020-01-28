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
  .aggregate(typifyJVM, typifyJS, circeTypify, json4sTypify, playjsonTypify, sjsTypify)
  .settings(
    publish := {},
    publishLocal := {},
    bintrayReleaseOnPublish in ThisBuild := false
  )

lazy val cats = Def.setting { "org.typelevel" %%% "cats-core" % "2.1.0" }
lazy val circe = "io.circe" %% "circe-core" % "0.12.3"
lazy val playJson = "com.typesafe.play" %% "play-json" % "2.6.10"
lazy val shapeless = Def.setting { "com.chuusai" %%% "shapeless" % "2.3.3" }
lazy val scalacheck = Def.setting { "org.scalacheck" %%% "scalacheck" % "1.14.2" % "test" }

lazy val typify = sbtcrossproject.CrossPlugin.autoImport.crossProject(JSPlatform, JVMPlatform).in(file("typify"))
  .settings(baseSettings)
  .settings(
    name := "typify",
    libraryDependencies ++= Seq(cats.value, shapeless.value, scalacheck.value),
    tutTargetDirectory := file("."),
    scalacOptions in Tut := (scalacOptions in (Compile, console)).value
  )
  .enablePlugins(TutPlugin)

lazy val typifyJVM = typify.jvm
lazy val typifyJS = typify.js.enablePlugins(ScalaJSPlugin)

lazy val circeTypify = project.in(file("circe-typify"))
  .settings(baseSettings)
  .settings(
    name := "circe-typify",
    libraryDependencies += circe,
  )
  .dependsOn(typifyJVM % "test->test;compile->compile")

lazy val json4sTypify = project.in(file("json4s-typify"))
  .settings(baseSettings)
  .settings(
    name := "json4s-typify",
    libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.6.7"
  )
  .dependsOn(typifyJVM % "test->test;compile->compile")

lazy val playjsonTypify = project.in(file("play-json-typify"))
  .settings(baseSettings)
  .settings(
    name := "play-json-typify",
    libraryDependencies ++= Seq(cats.value, playJson)
  )
  .dependsOn(typifyJVM % "test->test;compile->compile")

lazy val sjsTypify = project.in(file("jsdynamic-typify"))
  .settings(baseSettings)
  .settings(
    name := "jsdynamic-typify",
    scalaJSUseMainModuleInitializer := true
  )
  .dependsOn(typifyJS % "test->test;compile->compile")
  .enablePlugins(ScalaJSPlugin)
