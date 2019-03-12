val dottyVersion = "0.14.0-bin-20190311-4e62ca9-NIGHTLY"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "ch.epfl.lamp" % "dotty_0.14" % dottyVersion,
      "ch.epfl.lamp" % "dotty_0.14" % dottyVersion % "test->runtime",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
