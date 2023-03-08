import mill._, scalalib._, scalafmt._
import $ivy.`com.lihaoyi::mill-contrib-scalapblib:$MILL_VERSION`
import contrib.scalapblib.ScalaPBModule

object `scalapb-ujson` extends ScalaModule with ScalafmtModule {

  def scalaVersion = "3.2.2"

  def ivyDeps = Agg(
    ivy"com.lihaoyi::upickle:2.0.0",
    ivy"com.thesamet.scalapb::scalapb-runtime:0.11.11",
  )

  object test extends Tests with ScalaPBModule {
    def scalaPBLenses = false
    def scalaPBVersion = "0.11.11"
    def testFramework = "utest.runner.Framework"
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.7.10"
    )
  }

}
