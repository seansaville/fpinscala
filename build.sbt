lazy val commonSettings = Seq(
  organization := "uk.co.seansaville",
  scalaVersion := "2.12.0"
)

lazy val fpinscala = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "fpinscala"
  )
