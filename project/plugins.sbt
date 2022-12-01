addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.4.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.11.0")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.3.6")

resolvers += "bondlink-maven-repo" at "https://raw.githubusercontent.com/mblink/maven-repo/main"
addSbtPlugin("bondlink" % "sbt-git-publish" % "0.0.5")
