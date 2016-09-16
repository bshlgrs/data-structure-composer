name := "scala_big_o"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.3"
libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.1",
  "com.lihaoyi" %% "fastparse" % "0.3.7",
  "org.scalatest" % "scalatest_2.11" % "3.0.0"// % "test"
)
