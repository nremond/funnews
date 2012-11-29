import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "scalanews"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      // Redis driver
      "net.debasishg" % "redisclient_2.9.2" % "2.8",
      "org.sedis" % "sedis" % "1.0.1"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here      
    )

}
