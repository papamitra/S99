
import sbt._
import Process._
import Keys._

// Only needed to track snapshot releases, SBT automatically includes the releases repository.
resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

// NOTE: New Group ID from Scalaz 6.x and onwards
libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.3"
