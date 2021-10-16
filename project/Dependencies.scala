import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.9"
  lazy val cats = "org.typelevel" %% "cats-core" % "2.6.1"
  lazy val catsMtl = "org.typelevel" %% "cats-mtl" % "1.2.1"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "3.2.7"
  lazy val droste = "io.higherkindness" %% "droste-core" % "0.8.0"

  lazy val dependencies = Seq(
    scalaTest % Test,
    cats,
    catsMtl,
    catsEffect,
    droste,
  )

}
