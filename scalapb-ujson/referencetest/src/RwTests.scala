import utest.*

import java_protos.Protos
import protos.protos
import scalapb.ujson.JsonFormat

object RwTests extends TestSuite:

  def assertEqual(
    format: JsonFormat,
    message: protos.Message
  ): Unit =
    var printer = com.google.protobuf.util.JsonFormat.printer()
    if format.includeDefaultValueFields then printer = printer.includingDefaultValueFields()
    if format.preserveProtoFieldNames then printer = printer.preservingProtoFieldNames()
    if format.formatEnumsAsNumbers then printer = printer.printingEnumsAsInts()

    val scalaJson = ujson.read(format.write(message))
    val javaJson = ujson.read(printer.print(Protos.Message.parseFrom(message.toByteArray)))

    assert(scalaJson == javaJson)

  def assertEqualSpecial(
    format: JsonFormat,
    message: protos.SpecialFormats
  ): Unit =
    var printer = com.google.protobuf.util.JsonFormat.printer()
    if format.includeDefaultValueFields then printer = printer.includingDefaultValueFields()
    if format.preserveProtoFieldNames then printer = printer.preservingProtoFieldNames()
    if format.formatEnumsAsNumbers then printer = printer.printingEnumsAsInts()

    val scalaJson = ujson.read(format.write(message))
    val javaJson = ujson.read(printer.print(Protos.SpecialFormats.parseFrom(message.toByteArray)))

    assert(scalaJson == javaJson)

  val tests = Tests{
    test("empty") {
      val msg = protos.Message()
      test("no defaults"){
        val fmt = JsonFormat(includeDefaultValueFields = false)
        assertEqual(fmt, msg)
      }
      test("show defaults") {
        val fmt = JsonFormat(includeDefaultValueFields = true)
        assertEqual(fmt, msg)
      }
    }
    test("camel case") {
      val fmt = JsonFormat(includeDefaultValueFields = false, preserveProtoFieldNames = false)
      val msg = protos.Message(
        longNumber = 2,
        repeatedString = Seq("a", "b")
      )
      assertEqual(fmt, msg)
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
        assertEqual(fmt, msg)
      }
    }
    test("enum") {
      val msg = protos.Message(
        state = protos.Message.State.OK
      )
      test("name"){
        val fmt = JsonFormat(includeDefaultValueFields = false)
        assertEqual(fmt, msg)
      }
      test("number"){
        val fmt = JsonFormat(
          includeDefaultValueFields = false,
          formatEnumsAsNumbers = true
        )
        assertEqual(fmt, msg)
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
      assertEqual(fmt, msg)
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
      assertEqual(fmt, msg)
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
        assertEqual(fmt, msg)
      }
    }
    test("oneof") {
      val fmt = JsonFormat(
        includeDefaultValueFields = false
      )
      test("opt1") {
        val msg = protos.Message().withEither1("ok")
        assertEqual(fmt, msg)
      }
      test("opt2") {
        val msg = protos.Message().withEither2("ok")
        assertEqual(fmt, msg)
      }
      test("opt3") {
        val msg = protos.Message().withEither3(5)
        assertEqual(fmt, msg)
      }
      test("opt default") {
        val msg = protos.Message().withEither3(0)
        // even if defaults are not rendered, the choice needs to be kept
        assertEqual(fmt, msg)
      }
      test("opt default msg") {
        val msg = protos.Message().withEither4(protos.Message.Nested())
        // even if defaults are not rendered, the choice needs to be kept
        assertEqual(fmt, msg)
      }
    }
    test("binary") {
      val fmt = JsonFormat(
        includeDefaultValueFields = false
      )
      val msg = protos.Message(
        data = com.google.protobuf.ByteString.copyFromUtf8("hello, world")
      )
      assertEqual(fmt, msg)
    }
    test("optional") {
      val fmt = JsonFormat(
        includeDefaultValueFields = false
      )
      val msg = protos.Message(optint = Some(0))
      assertEqual(fmt, msg)
    }
    test("specials") {
      test("no defaults") {
        val fmt = JsonFormat(
          includeDefaultValueFields = false
        )

        val msg = protos.SpecialFormats()
        assertEqualSpecial(fmt, msg)
      }
      test("defaults") {
        val fmt = JsonFormat(
          includeDefaultValueFields = true
        )

        val msg = protos.SpecialFormats()
        assertEqualSpecial(fmt, msg)
      }
      test("values") {
        val fmt = JsonFormat(
          includeDefaultValueFields = false
        )

        val msg = protos.SpecialFormats()
          .withTs(
            com.google.protobuf.timestamp.Timestamp(1678372591, 42)
          )
          .withDuration(
            com.google.protobuf.duration.Duration(1000, 42)
          )
          .withFm(
            com.google.protobuf.field_mask.FieldMask(Seq("a", "b.c"))
          )
        assertEqualSpecial(fmt, msg)
      }
    }
  }
