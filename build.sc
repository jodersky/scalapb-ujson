import mill._, scalalib._, scalafmt._, scalajslib._, scalanativelib._, publish._
import $ivy.`com.lihaoyi::mill-contrib-scalapblib:$MILL_VERSION`
import contrib.scalapblib.ScalaPBModule

trait Publish extends PublishModule {
  def publishVersion = "0.3.0"
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
  def scalaVersion = "3.2.2"

  def ivyDeps = Agg(
    ivy"com.lihaoyi::ujson::3.0.0",
    ivy"com.thesamet.scalapb::scalapb-runtime::0.11.12",
    ivy"io.github.cquiroz::scala-java-time::2.5.0"
  )

  def artifactName = "scalapb-ujson"

  trait UTest extends TestModule {
    def testFramework = "utest.runner.Framework"
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.lihaoyi::utest::0.7.11"
    )
  }
  trait PbTest extends TestModule with ScalaPBModule {
    def scalaPBGrpc = false
    def scalaPBLenses = false
    def scalaPBVersion = "0.11.12"
    def scalaPBIncludePath = super.scalaPBIncludePath() ++ Seq(scalaPBUnpackProto())
  }
}

object `scalapb-ujson` extends Module {
  object jvm extends MainModule {
    def millSourcePath = super.millSourcePath / os.up
    object test extends Tests with UTest with PbTest
    object referencetest extends Tests with UTest with PbTest {
      def protos = T.sources(jvm.test.millSourcePath / "protobuf")
      def scalaPBSources = T.sources(protos())

      def generateJavaProtos = T {
        for (dir <- protos(); file <- os.list(dir.path)) {
          os.proc(
            "protoc",
            s"--proto_path=${dir.path}",
            s"--java_out=${T.dest}",
            s"-I", scalaPBUnpackProto().path,
            file
          ).call()
        }
        PathRef(T.dest)
      }

      def generatedSources = T {
        super.generatedSources() ++ Seq(generateJavaProtos())
      }

      def ivyDeps = super.ivyDeps() ++ Agg(
        ivy"com.google.protobuf:protobuf-java:3.22.3",
        ivy"com.google.protobuf:protobuf-java-util:3.22.3"
      )
    }
  }
  object js extends MainModule with ScalaJSModule {
    def scalaJSVersion = "1.13.0"
    def millSourcePath = super.millSourcePath / os.up
    object test extends Tests with UTest with PbTest
  }
  object native extends MainModule with ScalaNativeModule{
    def scalaNativeVersion = "0.4.10"
    def millSourcePath = super.millSourcePath / os.up
    object test extends Tests with UTest with PbTest
  }
}

object example extends ScalaModule with ScalaPBModule {
  def scalaVersion = "3.2.2"
  def scalaPBVersion = "0.11.12"
  def moduleDeps = Seq(`scalapb-ujson`.jvm)
}
