name := "tictactoe"
scalaVersion := "2.13.2"
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.typelevel" %% "cats-effect" % "2.1.3",
  "org.scalatest" %% "scalatest" % "3.1.1" % "test"
)
scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"
