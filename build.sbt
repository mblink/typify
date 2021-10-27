Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala213 = "2.13.6"

def scalaVersionSpecificFolders(srcName: String, srcBaseDir: java.io.File, scalaVersion: String): Seq[java.io.File] =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) => Seq(srcBaseDir / srcName / "scala-2.13")
    case _ => Seq()
  }

lazy val baseSettings = Seq(
  scalaVersion := scala213,
  crossScalaVersions := Seq(scala213),
  version := "5.2.0",
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.patch),
  scalacOptions ++= Seq("-Vimplicits", "-Vimplicits-verbose-tree"),
  scalacOptions --= Seq(
    "-language:existentials",
    "-language:experimental.macros",
    "-language:implicitConversions"
  ),
  Compile / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("main", baseDirectory.value, scalaVersion.value),
  Test / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("test", baseDirectory.value, scalaVersion.value),
  licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
  gitPublishDir := file("/src/maven-repo")
)

lazy val root = project.in(file("."))
  .aggregate(typifyJVM, typifyJS, circeTypify, json4sTypify, playjsonTypify, sjsTypify)
  .settings(baseSettings)
  .settings(
    publish := {},
    publishLocal := {},
    gitRelease := {}
  )

lazy val cats = Def.setting { "org.typelevel" %%% "cats-core" % "2.6.1" }
lazy val circe = "io.circe" %% "circe-core" % "0.14.1"
lazy val json4s = "org.json4s" %% "json4s-jackson" % "3.6.11"
lazy val playJson = "com.typesafe.play" %% "play-json" % "2.10.0-RC5"
lazy val shapeless = Def.setting { "com.chuusai" %%% "shapeless" % "2.3.7" }
lazy val scalacheck = Def.setting { "org.scalacheck" %%% "scalacheck" % "1.15.4" % "test" }

lazy val typify = sbtcrossproject.CrossPlugin.autoImport.crossProject(JSPlatform, JVMPlatform).in(file("typify"))
  .settings(baseSettings)
  .settings(
    name := "typify",
    libraryDependencies ++= Seq(cats.value, shapeless.value, scalacheck.value)
  )

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
    libraryDependencies += json4s
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
  .settings(name := "jsdynamic-typify")
  .dependsOn(typifyJS % "test->test;compile->compile")
  .enablePlugins(ScalaJSPlugin)

lazy val docs = project.in(file("typify-docs"))
  .settings(baseSettings)
  .settings(
    mdocOut := file("."),
    scalacOptions -= "-Xfatal-warnings",
    gitRelease := {}
  )
  .dependsOn(typifyJVM)
  .enablePlugins(MdocPlugin)
