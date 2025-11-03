Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala213 = "2.13.17"
lazy val scala3 = "3.3.3"

ThisBuild / crossScalaVersions := Seq(scala213, scala3)

// GitHub Actions config
val javaVersions = Seq(8, 11, 17, 21).map(v => JavaSpec.temurin(v.toString))

ThisBuild / githubWorkflowJavaVersions := javaVersions
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)
ThisBuild / githubWorkflowTargetBranches := Seq("master")

def isJava(v: Int) = s"matrix.java == '${javaVersions.find(_.version == v.toString).get.render}'"

ThisBuild / githubWorkflowBuild ++= Seq(
  WorkflowStep.Sbt(List("mimaReportBinaryIssues"), name = Some("Binary compatibility"), cond = Some(isJava(8))),
  WorkflowStep.Sbt(List("docs/mdoc"), name = Some("Build docs"), cond = Some(isJava(17))),
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
  version := "12.0.0",
  resolvers += "bondlink-maven-repo" at "https://maven.bondlink-cdn.com",
  mimaPreviousArtifacts := Set("typify" %%% name.value % "12.0.0"),
  libraryDependencies ++= foldScalaV(scalaVersion.value)(
    Seq(compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.4" cross CrossVersion.patch)),
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
  publishTo := Some("BondLink S3".at("s3://bondlink-maven-repo")),
)

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
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

lazy val cats = Def.setting("org.typelevel" %%% "cats-core" % "2.13.0")
lazy val circe = "io.circe" %% "circe-core" % "0.14.10"
lazy val formless = Def.setting("com.bondlink" %%% "formless" % "0.5.1")
lazy val json4s = "org.json4s" %% "json4s-jackson" % "4.0.7"
lazy val playJson = "org.playframework" %% "play-json" % "3.0.4"
lazy val shapeless = Def.setting("com.chuusai" %%% "shapeless" % "2.3.12")
lazy val scalacheck = Def.setting("org.scalacheck" %%% "scalacheck" % "1.18.1" % Test)

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
