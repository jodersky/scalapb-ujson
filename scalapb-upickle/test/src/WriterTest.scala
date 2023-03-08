import utest.*

import scalapb.upickle.JsonFormat

object WriterTest extends TestSuite:

  def assertEqual(
    format: JsonFormat,
    message: scalapb.GeneratedMessage,
    json: ujson.Value
  ): Unit =
    val asTree = format.writeToJson(message)
    assert(asTree == json) // json tree did not match

    val asString = format.writeToJsonString(message)
    assert(asString == json.render()) // json strings did not match

  val tests = Tests{
    test("empty") {
      val msg = protos.Message()
      test("no defaults"){
        val fmt = JsonFormat(includeDefaultValueFields = false)
        assertEqual(fmt, msg, ujson.Obj())
      }
      test("show defaults") {
        val fmt = JsonFormat(includeDefaultValueFields = true)
        val expected = ujson.Obj(
          "number" -> 0,
          "long_number" -> 0,
          "dbl" -> 0,
          "str" -> "",
          "flag" -> false,
          "state" -> "UNKNOWN",
          "nested" -> ujson.Obj(
            "inner" -> ujson.Obj(
              "payload" -> ""
            )
          ),
          "repeated_string" -> ujson.Arr(),
          "repeated_nested" -> ujson.Arr(),
          "messages" -> ujson.Obj(),
          "nested_map" -> ujson.Obj()
        )
        assertEqual(fmt, msg, expected)
      }
      test("camel case") {
        val fmt = JsonFormat(includeDefaultValueFields = true, preserveProtoFieldNames = false)
        val expected = ujson.Obj(
          "number" -> 0,
          "longNumber" -> 0,
          "dbl" -> 0,
          "str" -> "",
          "flag" -> false,
          "state" -> "UNKNOWN",
          "nested" -> ujson.Obj(
            "inner" -> ujson.Obj(
              "payload" -> ""
            )
          ),
          "repeatedString" -> ujson.Arr(),
          "repeatedNested" -> ujson.Arr(),
          "messages" -> ujson.Obj(),
          "nestedMap" -> ujson.Obj()
        )
        assertEqual(fmt, msg, expected)
      }
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
        val expected = ujson.Obj(
          "number" -> 42,
          "long_number" -> 42,
          "dbl" -> 2.3,
          "str" -> "hello, world",
          "flag" -> true
        )
        assertEqual(fmt, msg, expected)
      }
    }
    test("enum") {
      val msg = protos.Message(
        state = protos.Message.State.OK
      )
      test("name"){
        val fmt = JsonFormat(includeDefaultValueFields = false)
        val expected = ujson.Obj(
          "state" -> "OK"
        )
        assertEqual(fmt, msg, expected)
      }
      test("number"){
        val fmt = JsonFormat(
          includeDefaultValueFields = false,
          formatEnumsAsNumber = true
        )
        val expected = ujson.Obj(
          "state" -> 1
        )
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
      val expected = ujson.Obj(
        "nested" -> ujson.Obj(
          "inner" -> ujson.Obj(
            "payload" -> "hello, world"
          )
        )
      )
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
      val expected = ujson.Obj(
        "repeated_string" -> ujson.Arr(
          "hello", "world"
        ),
        "repeated_nested" -> ujson.Arr(
          ujson.Obj(
            "inner" -> ujson.Obj(
              "payload" -> "hello"
            )
          ),
          ujson.Obj(
            "inner" -> ujson.Obj(
              "payload" -> "world"
            )
          ),
          ujson.Obj()
        )
      )
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
        val expected = ujson.Obj(
          "messages" -> ujson.Obj(
            "1000" -> "hello",
            "1001" -> "world"
          ),
          "nested_map" -> ujson.Obj(
            "1" -> ujson.Obj(
              "inner" -> ujson.Obj(
                "payload" -> "hello"
              )
            ),
            "2" -> ujson.Obj(
              "inner" -> ujson.Obj(
                "payload" -> "world"
              )
            ),
            "3" -> ujson.Obj()
          )
        )
        assertEqual(fmt, msg, expected)
      }

      test("kv-pairs") {
        val fmt = JsonFormat(
          includeDefaultValueFields = false,
          formatMapEntriesAsKeyValuePairs = true
        )
        val expected = ujson.Obj(
          "messages" -> ujson.Arr(
            ujson.Obj(
              "key" -> 1000,
              "value" -> "hello"
            ),
            ujson.Obj(
              "key" -> 1001,
              "value" -> "world"
            )
          ),
          "nested_map" -> ujson.Arr(
            ujson.Obj(
              "key" -> 1,
              "value" -> ujson.Obj(
                "inner" -> ujson.Obj(
                  "payload" -> "hello"
                )
              )
            ),
            ujson.Obj(
              "key" -> 2,
              "value" -> ujson.Obj(
                "inner" -> ujson.Obj(
                  "payload" -> "world"
                )
              )
            ),
            ujson.Obj(
              "key" -> 3,
              "value" -> ujson.Obj()
            )
          )
        )
        assertEqual(fmt, msg, expected)
      }
    }
    test("oneof") {
      val fmt = JsonFormat(
        includeDefaultValueFields = false
      )
      test("opt1") {
        val msg = protos.Message().withEither1("ok")
        assertEqual(fmt, msg, ujson.Obj("either_1" -> "ok"))
      }
      test("opt2") {
        val msg = protos.Message().withEither2("ok")
        assertEqual(fmt, msg, ujson.Obj("either_2" -> "ok"))
      }
      test("opt3") {
        val msg = protos.Message().withEither3(5)
        assertEqual(fmt, msg, ujson.Obj("either_3" -> 5))
      }
      test("opt default") {
        val msg = protos.Message().withEither3(0)
        // even if defaults are not rendered, the choice needs to be kept
        assertEqual(fmt, msg, ujson.Obj("either_3" -> 0))
      }
      test("opt default msg") {
        val msg = protos.Message().withEither4(protos.Message.Nested())
        // even if defaults are not rendered, the choice needs to be kept
        assertEqual(fmt, msg, ujson.Obj("either_4" -> ujson.Obj()))
      }
    }
    test("basic") {
      val s = scalapb.upickle.JsonFormat(includeDefaultValueFields = false)
        .writeToJsonString(protos.Dummy(5))
      println(">>> " + s)
    }
  }
