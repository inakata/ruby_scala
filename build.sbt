organization := "com.github.inakata"

name := "ruby_scala"

version := "0.0.1"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-deprecation","-unchecked", "-Yrangepos")

initialCommands in console += {
  Iterator("com.github.inakata.ruby_scala._").map("import "+).mkString("\n")
}
