@main def main() =

  val fmt = scalapb.upickle.JsonFormat(formatMapEntriesAsKeyValuePairs = true, includeDefaultValueFields = false)

  val m = fmt.readJsonString[protos.Other](
    """{"yo": "false", "x": 2, "y": 2, "other2": {"a": "ok"}, "ints": [5, 6], "data": [{"key": 2, "value": "a"}], "bar": "a" }"""
  )

  println(fmt.writeToJsonString(m))
