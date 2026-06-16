import scala.util.chaining.*

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala213 = "2.13.18"
lazy val scala3 = "3.3.8"
lazy val scalaVersions = Seq(scala213, scala3)

ThisBuild / scalaVersion := scala3

// GitHub Actions config
val javaVersions = Seq(11, 17, 21, 25).map(v => JavaSpec.temurin(v.toString))

ThisBuild / githubWorkflowJavaVersions := javaVersions
ThisBuild / githubWorkflowArtifactUpload := false
ThisBuild / githubWorkflowBuildMatrixFailFast := Some(false)
ThisBuild / githubWorkflowTargetBranches := Seq("master")

def isJava(v: Int) = s"matrix.java == '${javaVersions.find(_.version == v.toString).get.render}'"

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Run(List("sbt test"), name = Some("Build project")),
  WorkflowStep.Run(List("sbt mimaReportBinaryIssues"), name = Some("Binary compatibility"), cond = Some(isJava(25))),
  WorkflowStep.Run(List("sbt mdoc"), name = Some("Build docs"), cond = Some(isJava(25))),
)

ThisBuild / githubWorkflowPublishTargetBranches := Seq()

def foldScalaV[A](scalaVersion: String)(_213: => A, _3: => A): A =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) => _213
    case Some((3, _)) => _3
  }

lazy val baseSettings = Seq(
  organization := "typify",
  version := "13.0.0",
  libraryDependencies ++= foldScalaV(scalaVersion.value)(
    Seq(compilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.4").cross(CrossVersion.patch))),
    Seq(),
  ),
  libraryDependencySchemes += "org.scala-native" %% "test-interface_native0.5" % VersionScheme.EarlySemVer,
  scalacOptions ++= foldScalaV(scalaVersion.value)(
    Seq("-Vimplicits-verbose-tree"),
    Seq(),
  ),
  scalacOptions --= Seq(
    "-language:existentials",
    "-language:experimental.macros",
    "-language:implicitConversions"
  ),
  licenses += License.Apache2,
  publish / skip := true,
  mimaFailOnNoPrevious := false,
)

baseSettings

lazy val publishSettings = Seq(
  publish / skip := false,
  s3PublishBucket := "bondlink-maven-repo",
  resolvers += "bondlink-maven-repo" at "https://maven.bondlink-cdn.com",
  mimaPreviousArtifacts := Set(organization.value %% name.value % "12.0.0"),
)

def baseProj(
  matrix: ProjectMatrix,
  nme: String,
  includeJVM: Boolean = true,
  includeJS: Boolean = true,
  includeNative: Boolean = true,
) =
  matrix
    .pipe(p => if (includeJVM) p.jvmPlatform(scalaVersions = scalaVersions) else p)
    .pipe(p => if (includeJS) p.jsPlatform(scalaVersions = scalaVersions) else p)
    .pipe(p => if (includeNative) p.nativePlatform(scalaVersions = scalaVersions) else p)
    .settings(baseSettings ++ Seq(name := nme))

lazy val cats = "org.typelevel" %% "cats-core" % "2.13.0"
lazy val circe = "io.circe" %% "circe-core" % "0.14.15"
lazy val formless = "com.bondlink" %% "formless" % "0.8.0"
lazy val json4s = "io.github.json4s" %% "json4s-jackson" % "4.1.1"
lazy val playJson = "org.playframework" %% "play-json" % "3.0.6"
lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.19.0" % Test

lazy val typify = baseProj(projectMatrix.in(file("typify")), "typify")
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(cats, formless, scalacheck),
  )

lazy val circeTypify = baseProj(projectMatrix.in(file("circe-typify")), "circe-typify", includeJS = false, includeNative = false)
  .settings(publishSettings)
  .settings(
    libraryDependencies += circe,
  )
  .dependsOn(typify % "test->test;compile->compile")

lazy val json4sTypify = baseProj(projectMatrix.in(file("json4s-typify")), "json4s-typify", includeJS = false, includeNative = false)
  .settings(publishSettings)
  .settings(
    libraryDependencies += json4s
  )
  .dependsOn(typify % "test->test;compile->compile")

lazy val playjsonTypify = baseProj(projectMatrix.in(file("play-json-typify")), "play-json-typify", includeJS = false, includeNative = false)
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(cats, playJson)
  )
  .dependsOn(typify % "test->test;compile->compile")

lazy val sjsTypify = baseProj(projectMatrix.in(file("jsdynamic-typify")), "jsdynamic-typify", includeJVM = false, includeNative = false)
  .settings(publishSettings)
  .dependsOn(typify % "test->test;compile->compile")

lazy val docs = projectMatrix.in(file("typify-docs"))
  .jvmPlatform(scalaVersions = Seq(scala3))
  .settings(baseSettings)
  .settings(
    mdocOut := file("."),
    scalacOptions -= "-Werror",
  )
  .dependsOn(typify)
  .enablePlugins(MdocPlugin)
