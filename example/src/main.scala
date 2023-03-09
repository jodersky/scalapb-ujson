
@main def main() =
  import protos.Request

  // the ScalaPB message
  val req = Request(
    name = "john smith",
    number = 42,
    state = Request.State.OK,
    attributes = Map(
        "attr1" -> Request.Attr("hello", "world"),
        "attr2" -> Request.Attr("foo", "bar")
    )
  )

  // convert to JSON string
  val json: String = scalapb.ujson.JsonFormat.write(req, 2)
  println(json)

  val input: String =
    """|
       |{
       |  "name": "john smith",
       |  "number": 42,
       |  "state": "OK",
       |  "attributes": {
       |    "attr1": {
       |      "a": "hello",
       |      "b": "world"
       |    },
       |    "attr2": {
       |      "a": "foo",
       |      "b": "bar"
       |    }
       |  }
       |}
       |""".stripMargin

  // parse JSON string as a ScalaPB message
  val message: Request = scalapb.ujson.JsonFormat.read[Request](input)
  println(message)
  // Request(
  //   john smith,
  //   42,
  //   OK,
  //   Map(
  //     attr1 -> Attr(hello,world,UnknownFieldSet(Map())),
  //     attr2 -> Attr(foo,bar,UnknownFieldSet(Map()))
  //   ),
  //   UnknownFieldSet(Map())
  // )
