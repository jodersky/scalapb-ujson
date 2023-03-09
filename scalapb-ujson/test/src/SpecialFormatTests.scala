import utest.*

import scalapb.ujson.JsonReadException
import scalapb.ujson.JsonFormat
import protos.SpecialFormats

object SpecialFormatTests extends TestSuite:

  val tests = Tests{
    test("special") {
      val msg = SpecialFormats(
        Some(com.google.protobuf.timestamp.Timestamp(2, 1))
      )
      println(JsonFormat().write(msg))

      println(JsonFormat().read[SpecialFormats]("""{"ts":"1970-01-01T00:00:02.000000001Z"}"""))

    }
  }
