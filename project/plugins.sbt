addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.3")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.4.2")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.13.0")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.3.7")

resolvers += "bondlink-maven-repo" at "https://raw.githubusercontent.com/mblink/maven-repo/main"
addSbtPlugin("bondlink" % "sbt-git-publish" % "0.0.5")
