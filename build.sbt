import typify._

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala213 = "2.13.10"
lazy val scala3 = "3.3.0"

def foldScalaV[A](scalaVersion: String)(_213: => A, _3: => A): A =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) => _213
    case Some((3, _)) => _3
  }

lazy val baseSettings = Seq(
  scalaVersion := scala3,
  crossScalaVersions := Seq(scala213, scala3),
  organization := "typify",
  version := "7.1.0",
  libraryDependencies ++= foldScalaV(scalaVersion.value)(
    Seq(compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.patch)),
    Seq(),
  ),
  scalacOptions ++= foldScalaV(scalaVersion.value)(
    Seq("-Vimplicits-verbose-tree"),
    Seq(
      "-no-indent",
      "-Wvalue-discard",
      "-Wunused:implicits",
      "-Wunused:imports",
      "-Wunused:locals",
      "-Wunused:params",
      "-Wunused:privates",
      "-Wunused:unsafe-warn-patvars",
    ),
  ),
  scalacOptions --= Seq(
    "-language:existentials",
    "-language:experimental.macros",
    "-language:implicitConversions"
  ),
  licenses += License.Apache2,
  gitPublishDir := file("/src/maven-repo")
)

lazy val root = project.in(file("."))
  .aggregate(tuple.jvm, tuple.js, typifyJVM, typifyJS, circeTypify, json4sTypify, playjsonTypify, sjsTypify)
  .settings(baseSettings)
  .settings(
    publish := {},
    publishLocal := {},
    gitRelease := {}
  )

lazy val cats = Def.setting { "org.typelevel" %%% "cats-core" % "2.9.0" }
lazy val circe = "io.circe" %% "circe-core" % "0.14.5"
lazy val json4s = "org.json4s" %% "json4s-jackson" % "4.0.6"
lazy val playJson = "com.typesafe.play" %% "play-json" % "2.10.0-RC9"
lazy val shapeless = Def.setting { "com.chuusai" %%% "shapeless" % "2.3.10" }
lazy val scalacheck = Def.setting { "org.scalacheck" %%% "scalacheck" % "1.17.0" % "test" }

lazy val tuple = crossProject(JSPlatform, JVMPlatform).in(file("tuple"))
  .settings(baseSettings)
  .settings(
    name := "typify-tuple",
    libraryDependencies ++= Seq(cats.value, scalacheck.value),
    libraryDependencies ++= foldScalaV(scalaVersion.value)(
      Seq(
        shapeless.value,
        scalaOrganization.value % "scala-compiler" % scalaVersion.value % "provided",
      ),
      Seq(),
    ),
    Test / sourceGenerators += Def.task {
      val srcManaged = (Test / sourceManaged).value / "generated"

      def gen(scalaF: String, generator: SourceGenerator) = {
        println(s"Generating ${srcManaged / scalaF} with $generator")
        IO.write(srcManaged / scalaF, generator())
        srcManaged / scalaF
      }

      Seq(
        gen("Util.scala", SourceGenerator.Util),
        gen("TupleSelectorTest.scala", SourceGenerator.TupleSelectorTest),
        gen("SelectorTest.scala", SourceGenerator.SelectorTest),
        gen("UpdaterTest.scala", SourceGenerator.UpdaterTest),
        gen("ModifierTest.scala", SourceGenerator.ModifierTest),
        gen("RenamerTest.scala", SourceGenerator.RenamerTest),
        gen("RemoverTest.scala", SourceGenerator.RemoverTest),
      )
    }
  )

lazy val typify = crossProject(JSPlatform, JVMPlatform).in(file("typify"))
  .settings(baseSettings)
  .settings(
    name := "typify",
    libraryDependencies ++= Seq(cats.value, scalacheck.value),
    libraryDependencies ++= foldScalaV(scalaVersion.value)(
      Seq(shapeless.value),
      Seq(),
    ),
  )
  .dependsOn(tuple)
  .aggregate(tuple)

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
