Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala213 = "2.13.10"
lazy val scala3 = "3.3.0"

ThisBuild / crossScalaVersions := Seq(scala213, scala3)

// GitHub Actions config
val javaVersions = Seq(8, 11, 17).map(v => JavaSpec.temurin(v.toString))

ThisBuild / githubWorkflowJavaVersions := javaVersions
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)
ThisBuild / githubWorkflowTargetBranches := Seq("master")

val isJava8 = s"matrix.java == '${javaVersions.find(_.version == "8").get.render}'"

ThisBuild / githubWorkflowBuild ++= Seq(
  WorkflowStep.Sbt(List("mimaReportBinaryIssues"), name = Some("Binary compatibility"), cond = Some(isJava8)),
  WorkflowStep.Sbt(List("docs/mdoc"), name = Some("Build docs"), cond = Some(isJava8)),
)

ThisBuild / githubWorkflowPublishTargetBranches := Seq()

def foldScalaV[A](scalaVersion: String)(_213: => A, _3: => A): A =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) => _213
    case Some((3, _)) => _3
  }

lazy val baseSettings = Seq(
  scalaVersion := scala3,
  crossScalaVersions := Seq(scala213, scala3),
  organization := "typify",
  version := "9.0.0",
  resolvers += "bondlink-maven-repo" at "https://raw.githubusercontent.com/mblink/maven-repo/main",
  mimaPreviousArtifacts := Set("typify" %%% name.value % "9.0.0"),
  libraryDependencies ++= foldScalaV(scalaVersion.value)(
    Seq(compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.patch)),
    Seq(),
  ),
  scalacOptions ++= foldScalaV(scalaVersion.value)(
    Seq("-Vimplicits-verbose-tree"),
    Seq(
      "-no-indent",
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

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  gitRelease := {},
)

lazy val root = project.in(file("."))
  .aggregate((
    typify.componentProjects ++
    Seq(circeTypify, json4sTypify, sjsTypify) ++
    (if (System.getProperty("java.version").startsWith("1.8")) Seq() else Seq(playjsonTypify))
  ).map(p => p: ProjectReference):_*)
  .settings(baseSettings)
  .settings(noPublishSettings)
  .disablePlugins(MimaPlugin)

lazy val cats = Def.setting("org.typelevel" %%% "cats-core" % "2.9.0")
lazy val circe = "io.circe" %% "circe-core" % "0.14.5"
lazy val formless = Def.setting("com.bondlink" %%% "formless" % "0.1.0")
lazy val json4s = "org.json4s" %% "json4s-jackson" % "4.0.6"
lazy val playJson = "com.typesafe.play" %% "play-json" % "2.10.0-RC9"
lazy val shapeless = Def.setting("com.chuusai" %%% "shapeless" % "2.3.10")
lazy val scalacheck = Def.setting("org.scalacheck" %%% "scalacheck" % "1.17.0" % Test)

lazy val typify = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("typify"))
  .settings(baseSettings)
  .settings(
    name := "typify",
    libraryDependencies ++= Seq(cats.value, formless.value, scalacheck.value),
  )

lazy val circeTypify = project.in(file("circe-typify"))
  .settings(baseSettings)
  .settings(
    name := "circe-typify",
    libraryDependencies += circe,
  )
  .dependsOn(typify.jvm % "test->test;compile->compile")

lazy val json4sTypify = project.in(file("json4s-typify"))
  .settings(baseSettings)
  .settings(
    name := "json4s-typify",
    libraryDependencies += json4s
  )
  .dependsOn(typify.jvm % "test->test;compile->compile")

lazy val playjsonTypify = project.in(file("play-json-typify"))
  .settings(baseSettings)
  .settings(
    name := "play-json-typify",
    libraryDependencies ++= Seq(cats.value, playJson)
  )
  .dependsOn(typify.jvm % "test->test;compile->compile")

lazy val sjsTypify = project.in(file("jsdynamic-typify"))
  .settings(baseSettings)
  .settings(name := "jsdynamic-typify")
  .dependsOn(typify.js % "test->test;compile->compile")
  .enablePlugins(ScalaJSPlugin)

lazy val docs = project.in(file("typify-docs"))
  .settings(baseSettings)
  .settings(noPublishSettings)
  .settings(
    mdocOut := file("."),
    scalacOptions -= "-Xfatal-warnings",
  )
  .dependsOn(typify.jvm)
  .enablePlugins(MdocPlugin)
  .disablePlugins(MimaPlugin)
