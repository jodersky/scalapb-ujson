import mill._, scalalib._, scalafmt._, scalajslib._, scalanativelib._, publish._
import $ivy.`com.lihaoyi::mill-contrib-scalapblib:$MILL_VERSION`
import contrib.scalapblib.ScalaPBModule

val scala3 = "3.2.2"
val scalajs = "1.12.0"
val scalanative = "0.4.10"
val scalapb = "0.11.13+7-601b2912-SNAPSHOT" //"0.11.12"

trait Publish extends PublishModule {
  def publishVersion = "0.1.1"
  def pomSettings = PomSettings(
    description = "scalapb-ujson",
    organization = "io.crashbox",
    url = "https://github.com/jodersky/scalapb-ujson",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("jodersky", "scalapb-ujson"),
    developers = Seq(
      Developer("jodersky", "Jakob Odersky", "https://github.com/jodersky")
    )
  )
}

trait MainModule extends ScalaModule with ScalafmtModule with Publish {
  def scalaVersion = scala3

  def ivyDeps = Agg(
    ivy"com.lihaoyi::upickle::2.0.0",
    ivy"com.thesamet.scalapb::scalapb-runtime::$scalapb",
    ivy"io.github.cquiroz::scala-java-time::2.5.0"
  )

  def artifactName = "scalapb-ujson"

  trait UTest extends TestModule with ScalaPBModule {
    def scalaPBGrpc = false
    def scalaPBLenses = false
    def scalaPBVersion = scalapb
    def testFramework = "utest.runner.Framework"
    def scalaPBIncludePath = super.scalaPBIncludePath() ++ Seq(scalaPBUnpackProto())
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.7.11"
    )
  }
}

object `scalapb-ujson` extends Module {
  object jvm extends MainModule {
    def millSourcePath = super.millSourcePath / os.up
    object test extends Tests with UTest
  }
  object js extends MainModule with ScalaJSModule {
    def scalaJSVersion = scalajs
    def millSourcePath = super.millSourcePath / os.up
    object test extends Tests with UTest
  }
  object native extends MainModule with ScalaNativeModule{
    def scalaNativeVersion = scalanative
    def millSourcePath = super.millSourcePath / os.up
    object test extends Tests with UTest
  }
}

object grpc extends ScalaModule {
  def scalaVersion = scala3
  def moduleDeps = Seq(`scalapb-ujson`.jvm)

  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"com.thesamet.scalapb::scalapb-runtime-grpc:$scalapb",
    ivy"io.undertow:undertow-core:2.3.4.Final",
    ivy"com.thesamet.scalapb.common-protos::proto-google-common-protos-scalapb_0.11:2.9.6-0",
  )

  object test extends Tests with ScalaPBModule {
    def scalaPBGrpc = true
    def scalaPBLenses = false
    def scalaPBVersion = scalapb
    def testFramework = "utest.runner.Framework"
    def scalaPBIncludePath = super.scalaPBIncludePath() ++ Seq(scalaPBUnpackProto())
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.7.11",
      ivy"io.grpc:grpc-netty:1.48.0"
    )
  }

  object core extends ScalaModule with ScalaPBModule {
    def scalaVersion = scala3
    def moduleDeps = Seq(`scalapb-ujson`.jvm)
    def scalaPBGrpc = true // testing
    def scalaPBLenses = false
    def scalaPBVersion = scalapb

    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.thesamet.scalapb::scalapb-runtime-grpc::$scalapb",
      ivy"io.grpc:grpc-netty:1.48.0"
    )
  }
}

object example extends ScalaModule with ScalaPBModule {
  def scalaVersion = scala3
  def scalaPBVersion = scalapb
  def moduleDeps = Seq(`scalapb-ujson`.jvm)
}
