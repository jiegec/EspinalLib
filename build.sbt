name := "EspinalLib"
version := "1.0"
crossScalaVersions := List("2.12.13", "2.13.6")
organization := "je.jia"

val spinalVersion = "1.6.1"

libraryDependencies ++= Seq(
  "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion,
  "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion,
  compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion),
  "org.scalatest" %% "scalatest-funsuite" % "3.2.3" % "test"
)

fork := true
