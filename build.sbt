name := "validated-examples"

organization := "validated"

version := "0.0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.typelevel"  %% "cats-core" % "1.6.1",
  "org.scalatest"  %% "scalatest"   % "3.0.8"  % "test",
  "org.scalacheck" %% "scalacheck"  % "1.14.0" % "test"
)

lazy val commonCompilerOptions =
  Seq(
      "-unchecked",
      "-encoding", "UTF-8",
      "-deprecation",
      "-feature",
      "-Ypartial-unification"
    )

scalacOptions ++= 
  commonCompilerOptions ++ 
  Seq(
      "-Xfatal-warnings",
      "-Ywarn-unused-import",
      "-Ywarn-infer-any",
      "-Ywarn-nullary-override",
      "-Ywarn-nullary-unit",
     )

scalacOptions in (Compile, console) := commonCompilerOptions

scalacOptions in (Test, console) := commonCompilerOptions

