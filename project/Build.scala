import sbt._
import Keys._

object MyBuild extends Build {
  lazy val deduction = Project("deduction", file(".")).
    dependsOn(parsing % "compile;test;test->test")
  lazy val parsing = RootProject( file("lib/parsing") )
}