lazy val commonSettings = Seq(
  organization := "dev.anonymous",
  scalaVersion := "2.12.8",
  addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  scalacOptions ++= Seq(
      "-Xlint",
      "-unchecked",
      "-deprecation",
      "-feature",
      "-Ypartial-unification",
      "-language:postfixOps",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-language:experimental.macros"),
  libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.2.18",
      "com.github.julien-truffaut" %% "monocle-core" % "1.5.0",
      "io.getquill" %% "quill-sql" % "3.1.0",
      "org.scalactic" %% "scalactic" % "3.0.5", 
      "org.scalatest" %% "scalatest" % "3.0.5" % "test")
)

lazy val core = project
  .settings(
    name := "optica-core",
    commonSettings
  )

lazy val example = project
  .dependsOn(core)
  .settings(
    name := "optica-example",
    commonSettings
  )

