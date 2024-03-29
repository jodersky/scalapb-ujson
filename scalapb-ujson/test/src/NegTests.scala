import utest.*

import scalapb.ujson.JsonReadException
import scalapb.ujson.JsonFormat
import protos.protos.Message

object NegTests extends TestSuite:

  val tests = Tests{
    test("wrong type") {
      val fmt = JsonFormat()

      val ex = intercept[JsonReadException](
        fmt.read[Message](
          """|{
             |  "number": "a"
             |}
             |""".stripMargin
        )
      )
      assert(ex.message == "field 'Message.number': string is not a valid number")
    }
    test("repeated") {
      val fmt = JsonFormat()

      val ex = intercept[JsonReadException](
        fmt.read[Message](
          """|{
             |  "number": ["a"]
             |}
             |""".stripMargin
        )
      )
      assert(ex.message == "protobuf message field 'Message.number' of type TYPE_INT32 does not accept a JSON array")
    }
    test("repeated2") {
      val fmt = JsonFormat()

      val ex = intercept[JsonReadException](
        fmt.read[Message](
          """|{
             |  "repeated_string": "a"
             |}
             |""".stripMargin
        )
      )
      assert(ex.message == "protobuf message field 'Message.repeated_string' of type repeated TYPE_STRING does not accept a JSON string")
    }
    test("repeated3") {
      val fmt = JsonFormat()

      val ex = intercept[JsonReadException](
        fmt.read[Message](
          """|{
             |  "repeated_string": {}
             |}
             |""".stripMargin
        )
      )
      assert(ex.message == "protobuf message field 'Message.repeated_string' of type repeated TYPE_STRING does not accept a JSON object")
    }
    test("default map") {
      val fmt = JsonFormat()

      test("wrong key type") {
        val ex = intercept[JsonReadException](
          fmt.read[Message](
            """|{
               |  "messages": {
               |    "a" : ""
               |  }
               |}
               |""".stripMargin
          )
        )
        assert(ex.message == "cannot parse key 'a' of map field 'Message.messages' as a Int")
      }
      test("wrong value type") {
        val ex = intercept[JsonReadException](
          fmt.read[Message](
            """|{
               |  "messages": {
               |    "1" : {}
               |  }
               |}
               |""".stripMargin
          )
        )
        assert(ex.message == "protobuf message field 'Message.MessagesEntry.value' of type TYPE_STRING does not accept a JSON object")
      }
      test("wrong type") {
        val ex = intercept[JsonReadException](
          fmt.read[Message](
            """|{
               |  "messages": []
               |}
               |""".stripMargin
          )
        )
        assert(ex.message == "protobuf message field 'Message.messages' of type map does not accept a JSON array")
      }
    }
  }
