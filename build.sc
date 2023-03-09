import mill._, scalalib._, scalafmt._, scalajslib._, scalanativelib._
import $ivy.`com.lihaoyi::mill-contrib-scalapblib:$MILL_VERSION`
import contrib.scalapblib.ScalaPBModule

trait MainModule extends ScalaModule with ScalafmtModule {
  def scalaVersion = "3.2.2"

  def ivyDeps = Agg(
    ivy"com.lihaoyi::upickle::2.0.0",
    ivy"com.thesamet.scalapb::scalapb-runtime::0.11.12",
    ivy"com.thesamet.scalapb.common-protos::proto-google-common-protos-scalapb_0.11:2.5.0-2"
  )

  trait UTest extends TestModule with ScalaPBModule {
    def scalaPBLenses = false
    def scalaPBVersion = "0.11.12"
    def testFramework = "utest.runner.Framework"
    def scalaPBIncludePath = super.scalaPBIncludePath() ++ Seq(scalaPBUnpackProto())
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.7.11"
    )
  }
}

object `scalapb-ujson` extends MainModule {
  object jvm extends MainModule {
    def millSourcePath = super.millSourcePath / os.up
    object test extends Tests with UTest
  }
  object js extends MainModule with ScalaJSModule {
    def scalaJSVersion = "1.12.0"
    def millSourcePath = super.millSourcePath / os.up
    object test extends Tests with UTest
  }
  object native extends MainModule with ScalaNativeModule{
    def scalaNativeVersion = "0.4.10"
    def millSourcePath = super.millSourcePath / os.up
    object test extends Tests with UTest
  }
}
