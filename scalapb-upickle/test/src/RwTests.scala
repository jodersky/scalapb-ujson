import utest.*

import scalapb.upickle.JsonFormat

object RwTests extends TestSuite:

  def assertEqual[A <: scalapb.GeneratedMessage](
    format: JsonFormat,
    message: A,
    json: String,
    checkRead: Boolean = true
  )(using companion: scalapb.GeneratedMessageCompanion[A]): Unit =
    val asTree = format.writeToJson(message)
    val tree = ujson.read(json)
    assert(asTree == tree) // json tree did not match

    val asString = format.write(message)
    val rerender = ujson.write(tree) // normalize any kind of indentation
    assert(asString == rerender) // json strings did not match

    val fromString: A = format.read[A](json)
    val fromTree: A = format.read[A](tree)

    if checkRead then
      assert(fromString == message)
      assert(fromTree == message)


  val tests = Tests{
    test("empty") {
      val msg = protos.Message()
      test("no defaults"){
        val fmt = JsonFormat(includeDefaultValueFields = false)
        assertEqual(fmt, msg, "{}")
      }
      test("show defaults") {
        val fmt = JsonFormat(includeDefaultValueFields = true)

        val expected =
          """|{
             |  "number": 0,
             |  "long_number": 0,
             |  "dbl": 0,
             |  "str": "",
             |  "flag": false,
             |  "state": "UNKNOWN",
             |  "nested": {
             |    "inner": {
             |      "payload": ""
             |    }
             |  },
             |  "repeated_string": [],
             |  "repeated_nested": [],
             |  "messages": {},
             |  "nested_map": {},
             |  "data": ""
             |}
             |""".stripMargin
        assertEqual(fmt, msg, expected, checkRead = false) // can't easily check reads with defaults included
      }
    }
    test("camel case") {
      val fmt = JsonFormat(includeDefaultValueFields = false, preserveProtoFieldNames = false)
      val msg = protos.Message(
        longNumber = 2,
        repeatedString = Seq("a", "b")
      )
      val expected =
        """|{
           |  "longNumber": 2,
           |  "repeatedString": ["a", "b"]
           |}
           |""".stripMargin
      assertEqual(fmt, msg, expected)

    }
    test("primitives") {
      val msg = protos.Message(
        number = 42,
        longNumber = 42L,
        dbl = 2.3,
        str = "hello, world",
        flag = true
      )
      test("no defaults"){
        val fmt = JsonFormat(includeDefaultValueFields = false)

        val expected =
          """|{
             |  "number": 42,
             |  "long_number": 42,
             |  "dbl": 2.3,
             |  "str": "hello, world",
             |  "flag": true
             |}
             |""".stripMargin
        assertEqual(fmt, msg, expected)
      }
    }
    test("enum") {
      val msg = protos.Message(
        state = protos.Message.State.OK
      )
      test("name"){
        val fmt = JsonFormat(includeDefaultValueFields = false)
        val expected = """{"state":"OK"}"""
        assertEqual(fmt, msg, expected)
      }
      test("number"){
        val fmt = JsonFormat(
          includeDefaultValueFields = false,
          formatEnumsAsNumbers = true
        )
        val expected = """{"state":1}"""
        assertEqual(fmt, msg, expected)
      }
    }
    test("nested") {
      val msg = protos.Message()
        .withNested(
          protos.Message.Nested()
            .withInner(protos.Message.Nested.Inner("hello, world"))
        )
      val fmt = JsonFormat(
        includeDefaultValueFields = false
      )
      val expected =
        """|{
           |  "nested": {
           |    "inner": {
           |      "payload": "hello, world"
           |    }
           |  }
           |}
           |""".stripMargin
      assertEqual(fmt, msg, expected)
    }

    test("repeated") {
      val nested1 = protos.Message.Nested()
        .withInner(protos.Message.Nested.Inner("hello"))

      val nested2 = protos.Message.Nested()
        .withInner(protos.Message.Nested.Inner("world"))

      val nested3 = protos.Message.Nested()

      val msg = protos.Message(
        repeatedString = Seq("hello", "world"),
        repeatedNested = Seq(nested1, nested2, nested3)
      )

      val fmt = JsonFormat(
        includeDefaultValueFields = false
      )
      val expected =
        """|{
           |  "repeated_string": [
           |    "hello",
           |    "world"
           |  ],
           |  "repeated_nested": [
           |    {
           |      "inner" : {
           |        "payload" : "hello"
           |      }
           |    },
           |    {
           |      "inner" : {
           |        "payload" : "world"
           |      }
           |    },
           |    {}
           |  ]
           |}
           |""".stripMargin
      assertEqual(fmt, msg, expected)
    }
    test("map") {
      val nested1 = protos.Message.Nested()
        .withInner(protos.Message.Nested.Inner("hello"))

      val nested2 = protos.Message.Nested()
        .withInner(protos.Message.Nested.Inner("world"))

      val nested3 = protos.Message.Nested()

      val msg = protos.Message(
        messages = Map(1000 -> "hello", 1001 -> "world"),
        nestedMap = Map(1 -> nested1, 2 -> nested2, 3 -> nested3)
      )

      test("direct") {
        val fmt = JsonFormat(
          includeDefaultValueFields = false
        )

        val expected =
          """|{
             |  "messages": {
             |    "1000": "hello",
             |    "1001": "world"
             |  },
             |  "nested_map": {
             |    "1": {
             |      "inner" : {
             |        "payload" : "hello"
             |      }
             |    },
             |    "2": {
             |      "inner" : {
             |        "payload" : "world"
             |      }
             |    },
             |    "3": {}
             |  }
             |}
             |""".stripMargin

        assertEqual(fmt, msg, expected)
      }

      test("kv-pairs") {
        val fmt = JsonFormat(
          includeDefaultValueFields = false,
          formatMapEntriesAsKeyValuePairs = true
        )
        val expected =
          """|{
             |  "messages": [
             |    {
             |      "key": 1000,
             |      "value": "hello"
             |    },
             |    {
             |      "key": 1001,
             |      "value": "world"
             |    }
             |  ],
             |  "nested_map": [
             |    {
             |      "key": 1,
             |      "value": {
             |        "inner": {
             |            "payload": "hello"
             |         }
             |      }
             |    },
             |    {
             |      "key": 2,
             |      "value": {
             |        "inner": {
             |            "payload": "world"
             |         }
             |      }
             |    },
             |    {
             |      "key": 3,
             |      "value": {}
             |    }
             |  ]
             |}
             |""".stripMargin
        assertEqual(fmt, msg, expected)
      }
    }
    test("oneof") {
      val fmt = JsonFormat(
        includeDefaultValueFields = false
      )
      test("opt1") {
        val msg = protos.Message().withEither1("ok")
        assertEqual(fmt, msg, """{"either_1": "ok"}""")
      }
      test("opt2") {
        val msg = protos.Message().withEither2("ok")
        assertEqual(fmt, msg, """{"either_2": "ok"}""")
      }
      test("opt3") {
        val msg = protos.Message().withEither3(5)
        assertEqual(fmt, msg, """{"either_3": 5}""")
      }
      test("opt default") {
        val msg = protos.Message().withEither3(0)
        // even if defaults are not rendered, the choice needs to be kept
        assertEqual(fmt, msg, """{"either_3": 0}""")
      }
      test("opt default msg") {
        val msg = protos.Message().withEither4(protos.Message.Nested())
        // even if defaults are not rendered, the choice needs to be kept
        assertEqual(fmt, msg, """{"either_4": {}}""")
      }
    }
    test("binary") {
      val fmt = JsonFormat(
        includeDefaultValueFields = false
      )
      val msg = protos.Message(
        data = com.google.protobuf.ByteString.copyFromUtf8("hello, world")
      )
      assertEqual(fmt, msg, """{"data":"aGVsbG8sIHdvcmxk"}""")
    }
  }
