# scalapb-ujson

[![stability: firm](https://img.shields.io/badge/stability-firm-silver)](https://www.crashbox.io/stability.html)

Read and write ScalaPB messages to and from JSON.

This project is a library that strives to implement the [Canonical Protobuf to
JSON Mapping](https://protobuf.dev/programming-guides/proto3/#json), for
ScalaPB-generated messages, using the ujson library.

## Example

Protobuf:

```protobuf
syntax = "proto3";

message Request {
  string name = 1;
  int32 number = 2;

  enum State {
    UNKNOWN = 0;
    OK = 1;
    ERROR = 2;
  }
  State state = 3;

  message Attr {
    string a = 1;
    string b = 2;
  }
  map<string, Attr> attributes = 4;
}
```

Scala:

```scala
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
```

The full example source code is contained in `example/`. You can run it with
`./mill example`.

## Documentation

**The central element of scalapb-ujson is the
[`scalapb.usjon.JsonFormat`](http://jodersky.github.io/scalapb-ujson/scalapb/ujson/JsonFormat$.html)
object.**

It has several `write` and `read` methods, which allow you to convert between
ScalaPB messages and JSON.

Have a look at the [API docs](http://jodersky.github.io/scalapb-ujson).

The conversion methods support writing and reading the resulting JSON to/from
various outputs/inputs. For example, the simplest `write` creates a string,
however there are others which create `ujson.Value` objects and others which
stream bytes directly without materializing the whole JSON structure in memory
first.

### Customizations

Some aspects of ScalaPB to JSON conversions can be customized as per [the
specification](https://protobuf.dev/programming-guides/proto3/#json-options).
These customizations are given as parameters to the `JsonFormat` companion
class. Hence, if you would like to customize the output, you can use a new
instance of `JsonFormat` rather than the default object.

For example, if you would like to hide default values in the JSON output:

```scala
val fmt = scalapb.ujson.JsonFormat(includeDefaultValueFields = false)

fmt.write(....)
```

Note that readers are not customizable. They are flexible and support reading
JSON from any customized output.

## Dependencies

This project tries to minimize dependencies, however, as a bridge between
libraries, some dependencies are required.

| Dependency | Reason |
|------------|--------|
| [ujson](https://github.com/com-lihaoyi/upickle) 3.x | One end of the conversion. Note that only the `ujson` and the core library is needed. There is NO dependency on the upickle macro library. |
| [scalapb-runtime](https://scalapb.github.io/) 0.11.x | The other end of the conversion. |
| [Scala Java Time](https://github.com/cquiroz/scala-java-time) 2.x | A Scala implementation of the `java.time` package. The time package is needed for serializing some well-known protocol buffers to JSON strings, and the Scala-implementation is needed to support cross-building for JS and native. |

## Scala Versions

This project works with Scala 3 on the JVM, JS and native. We try to be
conservative with the minimal required versions. As of this writing, the
following minimums are required:

- Scala: 3.2.2
- Scala JS: 1.12.0
- Scala Native: 0.4.10

## Maven Coordinates

The project is published to Maven Central:

```
ivy"io.crashbox::scalapb-ujson:0.2.0"
```

## Feature-Completeness

Most of the [Canonical Protobuf to JSON
Mapping](https://protobuf.dev/programming-guides/proto3/#json) is implemented,
including the writer options. Some advanced features are still lacking however,
and will be implemented on an as-is-needed basis by the authors. Contributions
are welcome however!

Missing features:

- special serialization of `google.protobuf.Any`
- special serialization of `ListValue, Value, Struct and NullValue`

Only Protocol Buffers version 3 has been tested.

## Why?

Protocol buffers are a binary format which makes them very efficient. However,
being binary also means that they are cumbersome to work-with in an ad-hoc
manner such as when building a quick prototype or during live debug
interactions. In these situations, which are crucial for developer productivity,
text-based formats always beat binary formats.

Therefore, by implementing a mapping between protobuf and JSON, the best of both
worlds can be achieved:

- you can use a language-agnostic IDL to keep an overview of your application's
  API
- you get the benefit of protobuf's efficiency
- you retain the ability to simply interact with your application in an ad-hoc
  way via JSON

scalapb-ujson strives to be very efficient and light on memory by fully
embracing ujson's visitor design. This means that JSON can be read and written
from various forms and doesn't need to go through an intermediate
representation.

### Alternatives

- [scalapb-json4s](https://github.com/scalapb/scalapb-json4s) is a direct
  inspiration for this project. It provides similar features, however it also
  diverges with this project in a couple of important points:

  - it uses json4s rather than ujson (which is the recommended JSON library from
    the Scala Toolkit)
  - it only works on the JVM

- [gRPC Gateway](https://github.com/grpc-ecosystem/grpc-gateway). scalapb-ujson
  integrates directly with ScalaPB and is not designed to be a converter of
  protobuf on the wire. In case you have a mature service architecture then gRPC
  gateway is a viable alternative. scalapb-ujson is only designed for standalone
  applications (and doesn't support gRPC service mapping).

## Building

- compile all modules: `./mill __.compile`
- run tests: `./mill __.test`
- run tests on only the main JVM project `./mill scalapb-ujson.jvm.test`
- run a specific test on all targets: `./mill __.test RwTests.empty`
- run tests important tests before publishing `ci/test`
- publish to maven central `ci/publish`

## Credits

Some helpers have been copied and adapted from
[scalapb-json4s](https://github.com/scalapb/scalapb-json4s), released under the
Apache 2.0 license. An appropriate notice is included in the relevant files.
