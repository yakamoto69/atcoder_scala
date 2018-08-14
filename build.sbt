lazy val commonSettings = Seq(
  scalaVersion := "2.11.7"
)

lazy val root = (project in file("."))
  .settings(
    commonSettings,
    name := "atcoder"
  )

lazy val util = (project in file("libs"))
  .settings(
    commonSettings
  )
