import mill._
import mill.scalalib._
import mill.scalalib.scalafmt._

object v {
  val scala2 = "2.13.12"
  val scala3 = "3.7.1"
}

trait WithScala2 extends ScalaModule{
  def scalaVersion = v.scala2
  def scalacOptions = Seq(
    "-unchecked",
    "-deprecation",
    "-language:reflectiveCalls",
    "-feature",
    "-Xcheckinit",
    "-Ywarn-dead-code",
    "-Ywarn-unused",
    "-Ymacro-annotations",
  )
}

trait WithScala3 extends ScalaModule{
  def scalaVersion = v.scala3
  def scalacOptions = Seq(
    "-unchecked",
    "-deprecation",
    "-language:reflectiveCalls",
    "-feature",
    "-Xcheckinit",
    "-Ywarn-dead-code",
    "-Ywarn-unused",
    "-Ymacro-annotations",
  )
}

object day1 extends WithScala3 
object day2 extends WithScala3 
