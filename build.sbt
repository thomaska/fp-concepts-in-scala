name := "tictactoe"
scalaVersion := "2.13.2"
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.typelevel" %% "cats-effect" % "2.1.3"
)
scalaSource in Compile := baseDirectory.value / "src"
