val scala3Version = "3.0.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "ESAT",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++=
      Seq(
        "com.novocode" % "junit-interface" % "0.11" % "test",
        "com.github.scopt" %% "scopt" % "4.0.1",
        "log4j" % "log4j" % "1.2.17"
      )
  )
