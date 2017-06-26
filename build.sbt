scalaVersion := "2.11.11"
wartremoverErrors ++= Warts.unsafe

lazy val root = project.in(file(".")).
  aggregate(typifyJS, typifyJVM, json4sTypify, sjsTypify, playjsonTypify).
  settings(
    publish := {},
    publishLocal := {},
    bintrayReleaseOnPublish in ThisBuild := false
  )

lazy val scalacF = Seq(
      "-deprecation",
      "-encoding", "UTF-8", // yes, this is 2 args
      "-feature",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ywarn-dead-code", // N.B. doesn't work well with the ??? hole
      "-Ywarn-infer-any",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused",
      "-Ywarn-value-discard",
      "-Xfuture")

lazy val typify = crossProject.in(file(".")).
  settings(
    name := "typify",
    version := "2.1.2",
    scalaVersion := "2.11.11",
    libraryDependencies ++= Seq(
      "com.chuusai" %%% "shapeless" % "2.3.2",
      "org.scalaz" %%% "scalaz-core" % "7.2.9",
      "org.scalacheck" %%% "scalacheck" % "1.12.6" % "test"
    ),
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    bintrayOrganization := Some("bondlink"),
    bintrayRepository := "Typify",
    bintrayReleaseOnPublish in ThisBuild := false,
    publishArtifact in Test := true
  ).
  jvmSettings(
    // Add JVM-specific settings here
    (Seq(scalacOptions ++= scalacF) ++ tutSettings):_*
  ).
  jsSettings(
    // Add JS-specific settings here
  )

lazy val typifyJVM = typify.jvm
lazy val typifyJS = typify.js.enablePlugins(ScalaJSPlugin)

lazy val json4sTypify = project.in(file("json4s-typify"))
  .dependsOn(typifyJVM % "test->test;compile->compile")
  .settings(
    name := "json4s-typify",
    version := "1.1.2",
    scalaVersion := "2.11.11",
    scalacOptions ++= scalacF,
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-jackson" % "3.5.0",
      "org.scalaz" %% "scalaz-core" % "7.2.9",
      "org.scalacheck" %% "scalacheck" % "1.12.6" % "test"
    ),
    bintrayOrganization := Some("bondlink"),
    bintrayRepository := "Typify",
    bintrayReleaseOnPublish in ThisBuild := false)

lazy val sjsTypify = project.in(file("jsdynamic-typify"))
  .dependsOn(typifyJS % "test->test;compile->compile")
  .settings(
    name := "jsdynamic-typify",
    version := "1.1.2",
    scalaVersion := "2.11.11",
    scalacOptions ++= scalacF,
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    libraryDependencies ++= Seq(
      "org.scalaz" %%% "scalaz-core" % "7.2.9",
      "org.scalacheck" %%% "scalacheck" % "1.12.6" % "test"
    ),
    scalaJSSemantics ~= { _.withAsInstanceOfs(
        org.scalajs.core.tools.sem.CheckedBehavior.Compliant) },
    bintrayOrganization := Some("bondlink"),
    bintrayRepository := "Typify",
    bintrayReleaseOnPublish in ThisBuild := false)
  .enablePlugins(ScalaJSPlugin)

lazy val playjsonTypify = project.in(file("play-json-typify"))
  .dependsOn(typifyJVM % "test->test;compile->compile")
  .settings(
    name := "play-json-typify",
    version := "1.2.0",
    scalaVersion := "2.11.11",
    scalacOptions ++= scalacF,
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.2.9",
      "com.typesafe.play" %% "play-json" % "2.6.0",
      "org.scalacheck" %% "scalacheck" % "1.12.6" % "test"
    ),
    bintrayOrganization := Some("bondlink"),
    bintrayRepository := "Typify",
    bintrayReleaseOnPublish in ThisBuild := false)

