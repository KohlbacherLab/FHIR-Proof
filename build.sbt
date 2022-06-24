


name := "fhir-proof"
organization in ThisBuild := "de.ekut.tbi"
scalaVersion in ThisBuild := "2.13.8"
version in ThisBuild := "0.1-SNAPSHOT"


//-----------------------------------------------------------------------------
// PROJECT
//-----------------------------------------------------------------------------

lazy val root = project.in(file("."))
  .settings(settings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest"     %% "scalatest" % "3.1.1" % Test,
      "org.typelevel"     %% "cats-core" % "2.1.1",
      "com.typesafe.play" %% "play-json" % "2.8.1",
      "com.chuusai"       %% "shapeless" % "2.3.3"
   )
 )


//-----------------------------------------------------------------------------
// SETTINGS
//-----------------------------------------------------------------------------

lazy val settings = commonSettings

lazy val compilerOptions = Seq(
  "-encoding", "utf8",
  "-unchecked",
  "-Xfatal-warnings",
  "-feature",
//  "-language:existentials",
  "-language:higherKinds",
//  "-language:implicitConversions",
  "-language:postfixOps",
  "-deprecation"
)

lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  resolvers ++= Seq(
    "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  )
)

