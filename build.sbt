ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.5"

lazy val commonSettings = Seq(
    libraryDependencies += "org.typelevel"    %% "cats-core"   % "2.13.0",
    libraryDependencies += "org.typelevel"    %% "cats-effect"   % "3.5.7",
  scalacOptions ++= Seq(
    "-source:future",
    "-Yretain-trees",
    "-Xmax-inlines",
    "64"
  ),
)

lazy val macros = (project in file("macros"))
  .settings(commonSettings)
  .settings(
    name := "app-macros",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.16"
  )

lazy val app = (project in file("app"))
  .dependsOn(macros)
  .settings(commonSettings)
  .settings(
      name := "error-example-app"
  )

lazy val root = (project in file("."))
  .aggregate(
    app,
    macros
  )
  .settings(
    name := "error-example",
  )
