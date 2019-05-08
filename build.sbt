lazy val commonSettings = Seq(
  scalaVersion := "2.11.7",
  scalacOptions += "-optimise"
)

lazy val root = (project in file("."))
  .settings(
    commonSettings,
    name := "atcoder"
  )

lazy val libs = (project in file("libs"))
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.5" % Test,
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
    )
  )

lazy val tests = (project in file("tests"))
  .settings(
    commonSettings
  ).dependsOn(root, libs)
