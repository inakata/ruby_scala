organization := "jp.scala_users.org"

name := "ruby_scala"

version := "0.0.1"

scalaVersion := "2.10.3"

seq(assemblySettings: _*)

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.3.1" % "test"
)
  
resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots", "releases"  at "http://oss.sonatype.org/content/repositories/releases")

publishTo := Some(Resolver.file("Github Pages", Path.userHome / "git" / "kmizu.github.com" / "maven" asFile)(Patterns(true, Resolver.mavenStyleBasePattern)))

publishMavenStyle := true

scalacOptions ++= Seq("-deprecation","-unchecked", "-Yrangepos")
 
initialCommands in console += {
  Iterator("org.scala_users.jp.ruby_scala._").map("import "+).mkString("\n")
}
