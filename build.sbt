name := "EspinalLib"
version := "1.0"
scalaVersion := "2.11.12"
organization := "je.jia"

val spinalVersion = "1.5.1"

libraryDependencies ++= Seq(
  "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion,
  "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion,
  compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion),
  "org.scalatest" %% "scalatest-funsuite" % "3.2.3" % "test"
)

fork := true
