lazy val root = project.in(file(".")).
  aggregate(typifyJS, typifyJVM).
  settings(
    publish := {},
    publishLocal := {}
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
    version := "1.0",
    scalaVersion := "2.11.8",
    libraryDependencies ++= Seq(
      "com.chuusai" %%% "shapeless" % "2.3.0",
      "org.scalaz" %%% "scalaz-core" % "7.2.2"
    )
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
  .dependsOn(typifyJVM)
  .settings(
    name := "json4s-typify",
    version := "1.0",
    scalaVersion := "2.11.8",
    scalacOptions ++= scalacF,
    libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-jackson" % "3.3.0",
      "org.scalaz" %% "scalaz-core" % "7.2.2"
    ))
