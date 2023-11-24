lazy val sharedSettings = Seq(
  version := "0.0.1-SNAPSHOT",
  organization := "com.github.sophiecollard",
  organizationName := "Sophie Collard",
  scalaVersion := "3.3.1",
  scalacOptions ++= Seq(
    "-deprecation",
//    "-Xfatal-warnings",
//    "-Ywarn-dead-code",
//    "-Ywarn-numeric-widen",
//    "-Ywarn-value-discard",
//    "-Ywarn-unused"
  )
)

lazy val core = (project in file("core"))
  .settings(
    sharedSettings,
    name := "core",
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.scalatest" %% "scalatest" % "3.2.17" % "test"
    )
  )

lazy val json = (project in file("json"))
  .dependsOn(core)
  .settings(
    sharedSettings,
    name := "json",
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-core" % "2.10.0",
      "org.scalatest" %% "scalatest" % "3.2.17" % "test"
    )
  )
