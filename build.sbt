ThisBuild / scalaVersion := "3.3.4"
ThisBuild / version      := "0.1.0"
ThisBuild / organization := "io.github.gogoasac"

val catsEffectVersion = "3.5.7"
val doobieVersion     = "1.0.0-RC5"
val mysqlVersion      = "8.0.33"
val munitVersion      = "1.0.0"

lazy val root = (project in file("."))
  .settings(
    name := "database-viewer-cli",
    libraryDependencies ++= Seq(
      "org.typelevel"  %% "cats-effect"         % catsEffectVersion,
      "org.tpolecat"   %% "doobie-core"         % doobieVersion,
      "org.tpolecat"   %% "doobie-hikari"       % doobieVersion,
      "mysql"           % "mysql-connector-java" % mysqlVersion,
      "org.scalameta"  %% "munit"                % munitVersion % Test,
    ),
    // Ensure stdout is not buffered so ANSI codes render immediately
    fork                    := true,
    javaOptions            ++= Seq(
      "-Dfile.encoding=UTF-8",
      "-Djansi.passthrough=true"
    ),
    Compile / mainClass     := Some("Main")
  )
