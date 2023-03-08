package scalapb.upickle

import upickle.core.Visitor
import upickle.core.ArrVisitor
import upickle.core.ObjVisitor
import scalapb.descriptors as sd
import ujson.Transformer

class JsonFormatException(msg: String, cause: Exception = null)
    extends Exception(msg, cause)

// something went wrong reading JSON as a protobuf message
class JsonReadException(
    val message: String,
    val position: Int,
    cause: Exception = null
) extends JsonFormatException(s"$message (position: $position)", cause)

object JsonFormat:

  /** `this_is_snake_case => thisIsCamelCase` */
  def camelify(snake: String): String =
    val camel = new StringBuilder
    var prevIsUnder = false
    for c <- snake do
      if prevIsUnder then
        camel += c.toUpper
        prevIsUnder = false
      else if c == '_' then prevIsUnder = true
      else
        camel += c
        prevIsUnder = false
    camel.result()

  def defaultPrimitiveValue(fd: sd.FieldDescriptor): sd.PValue =
    import sd.ScalaType
    require(fd.isOptional)
    fd.scalaType match {
      case ScalaType.Int     => sd.PInt(0)
      case ScalaType.Long    => sd.PLong(0L)
      case ScalaType.Float   => sd.PFloat(0)
      case ScalaType.Double  => sd.PDouble(0)
      case ScalaType.Boolean => sd.PBoolean(false)
      case ScalaType.String  => sd.PString("")
      case ScalaType.ByteString =>
        sd.PByteString(com.google.protobuf.ByteString.EMPTY)
      case ScalaType.Enum(ed) => sd.PEnum(ed.values(0))
      case ScalaType.Message(_) =>
        throw JsonFormatException(
          "no default value for a message; it is automatically constructed when writing a message"
        )
    }

/** Utility for writing and reading ScalaPB-generated messages to and from JSON
  * via the ujson library.
  *
  * This utility is designed around ujson's visitors. This means that
  * intermediate data structures are avoided as much as reasonably possible, and
  * therefore memory usage is kept low and performance should be very good for
  * conversions. For example, if you write a ScalaPB message to a JSON string,
  * the message will be transformed into a string directly, without passing
  * through an intermediate JSON tree structure. Or, if you write a message to a
  * stream, the message is written on demand and no full JSON string needs to be
  * generated beforehand. // TODO: check this latter claim, I'm not sure if
  * upickle's renderers actually uphold this
  *
  * @param preserveProtoFieldNames
  *   Default true. If set, then the field names of protobuf messages are used
  *   as-is (this means usually snake_case). Otherwise, they are converted to
  *   camelCase.
  *
  * @param includeDefaultValueFields
  *   Default true. If set, then fields of messages are always included in JSON,
  *   even if they are not set or correspond to the default protobuf value.
  *
  * @param formatEnumsAsNumbers
  *   Default false. Use enum's numbers instead of their names.
  *
  * @param formatMapEntriesAsKeyValuePairs
  *   Default false. By default, maps are serialized as JSON objects, with the
  *   JSON keys being the stringified form of the protobuf keys (protobuf only
  *   allows primitive types in map keys). If set, this will instead serialize
  *   maps as objects with `{"key":..., "value":...}` attributes.
  */
class JsonFormat(
    val preserveProtoFieldNames: Boolean = true,
    val includeDefaultValueFields: Boolean = true,
    val formatEnumsAsNumbers: Boolean = false,
    val formatMapEntriesAsKeyValuePairs: Boolean = false
):

  def jsonName(fd: sd.FieldDescriptor): String =
    if preserveProtoFieldNames then fd.asProto.getName
    else
      // protoc<3 doesn't know about json_name, so we fill it in if it's not populated.
      fd.asProto.jsonName
        .getOrElse(JsonFormat.camelify(fd.asProto.getName))

  def write[V](
      out: Visitor[_, V],
      message: scalapb.GeneratedMessage
  ): V =
    writeMessage(
      out,
      message.companion.scalaDescriptor.fields,
      message.toPMessage
    )

  def writeToJsonString(
      message: scalapb.GeneratedMessage,
      indent: Int = -1
  ): String =
    val sb = new java.io.StringWriter
    write(ujson.Renderer(sb, indent), message)
    sb.toString()
  end writeToJsonString

  def writeToJson(
      message: scalapb.GeneratedMessage,
      indent: Int = -1
  ): ujson.Value =
    write(ujson.Value, message)

  def writeMessage[V](
      out: Visitor[_, V],
      orderedFields: Seq[
        sd.FieldDescriptor
      ], // PMessage doesn't have a field order, so we pass it in externally
      message: sd.PMessage
  ): V =
    val fields = message.value
    val objVisitor = out.visitObject(
      length = fields.size,
      jsonableKeys = true, // no idea what this does,
      -1
    )

    for descriptor <- orderedFields do
      val value = fields(descriptor)
      writeField(objVisitor, descriptor, value)

    objVisitor.visitEnd(-1)

  def writeField(
      out: ObjVisitor[_, _],
      fd: sd.FieldDescriptor,
      value: sd.PValue
  ): Unit = value match
    case sd.PEmpty =>
      if includeDefaultValueFields && fd.containingOneof == None then
        out.visitKeyValue(out.visitKey(-1).visitString(jsonName(fd), -1))

        // This is a bit of a trick: in ScalaPB, PEmpty is only used for message
        // fields, so this check would be redundant. However, in order to avoid
        // code duplication, we recursively call this function with PEmpty
        // meaning the absence of a value, even for primitive types. This allows
        // us to recursively construct nested default messages, without the need
        // of duplicating logic in a separate function.
        if fd.protoType.isTypeMessage then
          val sd.ScalaType.Message(md) = (fd.scalaType: @unchecked)

          out.narrow.visitValue(
            writeMessage(
              out.subVisitor,
              md.fields,
              sd.PMessage(
                md.fields.map(f => f -> sd.PEmpty).toMap
              ) // here PEmpty is not necessarily a missing *message* type
            ),
            -1
          )
        else
          out.narrow.visitValue(
            writePrimitive(
              out.subVisitor,
              fd,
              JsonFormat.defaultPrimitiveValue(fd)
            ),
            -1
          )
    case sd.PRepeated(xs) =>
      if xs.nonEmpty || includeDefaultValueFields then
        out.visitKeyValue(out.visitKey(-1).visitString(jsonName(fd), -1))

        if fd.isMapField && !formatMapEntriesAsKeyValuePairs then
          val mapEntryDescriptor =
            fd.scalaType.asInstanceOf[sd.ScalaType.Message].descriptor
          val keyDescriptor = mapEntryDescriptor.findFieldByNumber(1).get
          val valueDescriptor = mapEntryDescriptor.findFieldByNumber(2).get

          val objv = out.subVisitor.visitObject(xs.size, true, -1)
          for x <- xs do
            val kv = x.asInstanceOf[sd.PMessage]

            val key = kv.value(keyDescriptor) match
              case sd.PBoolean(v) => v.toString
              case sd.PDouble(v)  => v.toString
              case sd.PFloat(v)   => v.toString
              case sd.PInt(v)     => v.toString
              case sd.PLong(v)    => v.toString
              case sd.PString(v)  => v
              case v =>
                throw new JsonFormatException(s"Unexpected value for key: $v")

            objv.visitKeyValue(objv.visitKey(-1).visitString(key, -1))

            if valueDescriptor.protoType.isTypeMessage then
              val sd.ScalaType.Message(md) =
                (valueDescriptor.scalaType: @unchecked)
              objv.narrow.visitValue(
                writeMessage(
                  objv.narrow.subVisitor,
                  md.fields,
                  kv.value(valueDescriptor).asInstanceOf[sd.PMessage]
                ),
                -1
              )
            else
              objv.narrow.visitValue(
                writePrimitive(
                  objv.narrow.subVisitor,
                  valueDescriptor,
                  kv.value(valueDescriptor)
                ),
                -1
              )
          end for
          out.narrow.visitValue(objv.visitEnd(-1), -1)
        else if fd.protoType.isTypeMessage then
          val sd.ScalaType.Message(md) = (fd.scalaType: @unchecked)
          val arrv = out.subVisitor.visitArray(xs.size, -1)
          for x <- xs do
            arrv.narrow.visitValue(
              writeMessage(
                arrv.subVisitor,
                md.fields,
                x.asInstanceOf[sd.PMessage]
              ),
              -1
            )
          out.narrow.visitValue(arrv.visitEnd(-1), -1)
        else
          val arrv = out.subVisitor.visitArray(xs.size, -1)
          for x <- xs do
            arrv.narrow.visitValue(
              writePrimitive(arrv.narrow.subVisitor, fd, x),
              -1
            )
          out.narrow.visitValue(arrv.visitEnd(-1), -1)

    case msg: sd.PMessage =>
      out.visitKeyValue(out.visitKey(-1).visitString(jsonName(fd), -1))

      val sd.ScalaType.Message(md) = (fd.scalaType: @unchecked)
      out.narrow.visitValue(
        writeMessage(
          out.subVisitor,
          md.fields,
          msg
        ),
        -1
      )

    case other =>
      if includeDefaultValueFields || !fd.isOptional || !fd.file.isProto3 || JsonFormat
          .defaultPrimitiveValue(fd) != other || fd.containingOneof.isDefined
      then
        out.visitKeyValue(out.visitKey(-1).visitString(jsonName(fd), -1))
        out.narrow.visitValue(
          writePrimitive(out.subVisitor, fd, other),
          -1
        )

  private inline def unsignedInt(n: Int): Long = n & 0x00000000ffffffffL

  def writePrimitive[V](
      out: Visitor[_, V],
      fd: sd.FieldDescriptor,
      value: sd.PValue
  ): V =
    value match {
      case sd.PEnum(e) =>
        // config.formatRegistry.getEnumWriter(e.containingEnum) match {
        //   case Some(writer) => writer(this, e)
        //   case None =>
        //     if (config.isFormattingEnumsAsNumber) JInt(e.number)
        //     else JString(e.name)
        // }
        if formatEnumsAsNumbers then out.visitInt32(e.number, -1)
        else out.visitString(e.name, -1)

      case sd.PInt(v) if fd.protoType.isTypeUint32 =>
        out.visitInt64(unsignedInt(v), -1)
      case sd.PInt(v) if fd.protoType.isTypeFixed32 =>
        out.visitInt64(unsignedInt(v), -1)
      case sd.PInt(v) =>
        out.visitInt32(v, -1)
      case sd.PLong(v) if fd.protoType.isTypeUint64 =>
        out.visitUInt64(v, -1)
      case sd.PLong(v) if fd.protoType.isTypeFixed64 =>
        out.visitUInt64(v, -1)
      case sd.PLong(v) =>
        out.visitInt64(v, -1)
      case sd.PDouble(v) =>
        out.visitFloat64(v, -1)
      case sd.PFloat(v) =>
        out.visitFloat32(v, -1)
      case sd.PBoolean(v) =>
        if v then out.visitTrue(-1) else out.visitFalse(-1)
      case sd.PString(v) =>
        out.visitString(v, -1)
      case sd.PByteString(v) =>
        val bytes = v.toByteArray()
        out.visitString(
          java.util.Base64.getEncoder().encodeToString(bytes),
          -1
        )
      case _: sd.PMessage | sd.PRepeated(_) | sd.PEmpty =>
        throw new RuntimeException("should not happen")
    }

  def readJson[A <: scalapb.GeneratedMessage](json: ujson.Value)(using
      companion: scalapb.GeneratedMessageCompanion[A]
  ): A =
    val pmessage = ujson.transform(json, Reader(companion.scalaDescriptor))
    companion.messageReads.read(pmessage)

  def readJsonString[A <: scalapb.GeneratedMessage](json: String)(using
      companion: scalapb.GeneratedMessageCompanion[A]
  ): A =
    val pmessage = ujson.transform(json, Reader(companion.scalaDescriptor))
    companion.messageReads.read(pmessage)

  class Reader(md: sd.Descriptor) extends SimpleVisitor[sd.PValue, sd.PMessage]:
    override val expectedMsg: String = "expected JSON object"
    override def visitObject(
        length: Int,
        jsonableKeys: Boolean,
        index: Int
    ): ObjVisitor[sd.PValue, sd.PMessage] =
      MessageReader(md, false)

  class MessageReader(md: sd.Descriptor, kvOnly: Boolean)
      extends ObjVisitor[sd.PValue, sd.PMessage]:
    private val parsedFields =
      collection.mutable.Map.empty[sd.FieldDescriptor, sd.PValue]

    private var key: String = null
    private var keyIndex: Int = -1
    private val fv = FieldVisitor(null)
    private val fieldMap = md.fields.map { field =>
      jsonName(field) -> field
    }.toMap

    override def visitKey(index: Int): Visitor[?, ?] =
      keyIndex = index
      KeyVisitor

    override def visitKeyValue(v: Any): Unit =
      key = v.asInstanceOf[String]
      fieldMap.get(key) match
        case Some(d) => fv.fd = d
        case None    => fv.fd = null
      if kvOnly && !(key == "key" || key == "value") then
        throw JsonReadException(
          s"only JSON objects with keys 'key' and 'value' are allowed in maps; found '$key'",
          keyIndex
        )

    override def subVisitor: Visitor[?, ?] =
      if fv.fd == null then NoOpVisitor else fv

    override def visitValue(v: sd.PValue, index: Int): Unit =
      if v != sd.PEmpty then parsedFields += fv.fd -> v

    override def visitEnd(index: Int): sd.PMessage =
      sd.PMessage(parsedFields.toMap)

  end MessageReader

  // PMessage already does error checking, but the messages aren't friendly
  class FieldVisitor(var fd: sd.FieldDescriptor, inArray: Boolean = false)
      extends ujson.JsVisitor[sd.PValue, sd.PValue]:

    private def unexpectedType(tpe: String, index: Int) =
      val fieldTpe =
        if fd.isMapField then "map"
        else if fd.isRepeated then s"repeated ${fd.protoType}"
        else fd.protoType
      throw JsonReadException(
        s"Protobuf message field '${fd.fullName}' of type ${fieldTpe} does not accept a JSON ${tpe}",
        index
      )

    // repeated fields are of the same base type as singular ones
    private def checkNotRepeated(tpe: String, index: Int) =
      if !inArray && fd.isRepeated then unexpectedType(tpe, index)

    private def visitBool(value: Boolean, index: Int) =
      checkNotRepeated("boolean", index)
      if fd.protoType.isTypeBool then sd.PBoolean(value)
      else unexpectedType("boolean", index)

    override def visitTrue(index: Int) = visitBool(true, index)
    override def visitFalse(index: Int) = visitBool(false, index)

    // TODO: improve precision loss
    private def parseNumber(str: String, index: Int) =
      val pt = fd.protoType

      val asDouble = str.toDouble
      val asLong = asDouble.toLong

      if pt.isTypeInt32 || pt.isTypeSint32 || pt.isTypeUint32 || pt.isTypeFixed32 || pt.isTypeSfixed32
      then sd.PInt(asLong.toInt)
      else if pt.isTypeInt64 || pt.isTypeSint64 || pt.isTypeUint64 || pt.isTypeFixed64 || pt.isTypeSfixed64
      then sd.PLong(asLong)
      else if pt.isTypeDouble then sd.PDouble(asDouble)
      else if pt.isTypeDouble then sd.PFloat(asDouble.toFloat)
      else if pt.isTypeEnum && formatEnumsAsNumbers then
        val sd.ScalaType.Enum(ed) = (fd.scalaType: @unchecked)
        ed.findValueByNumber(asLong.toInt) match
          case None     => sd.PEmpty // ignore unknown value
          case Some(ev) => sd.PEnum(ev)
      else unexpectedType("number", index)

    override def visitFloat64StringParts(
        s: CharSequence,
        decIndex: Int,
        expIndex: Int,
        index: Int
    ) =
      checkNotRepeated("number", index)
      parseNumber(s.toString, index)

    override def visitString(s: CharSequence, index: Int) =
      checkNotRepeated("string", index)

      if fd.protoType.isTypeEnum && !formatEnumsAsNumbers then
        val sd.ScalaType.Enum(ed) = (fd.scalaType: @unchecked)
        ed.values.find(_.name == s.toString) match
          case None     => sd.PEmpty // ignore unknown value
          case Some(ev) => sd.PEnum(ev)
      else if fd.protoType.isTypeString then sd.PString(s.toString())
      else if fd.protoType.isTypeBytes then
        sd.PByteString(
          com.google.protobuf.ByteString.copyFrom(
            java.util.Base64.getDecoder().decode(s.toString)
          )
        )
      else unexpectedType("string", index)

    override def visitNull(index: Int) =
      sd.PEmpty // we treat null as an omitted field

    override def visitJsonableObject(
        length: Int,
        index: Int
    ): ObjVisitor[sd.PValue, sd.PValue] =
      if fd.isMapField && !formatMapEntriesAsKeyValuePairs then
        MapReader(length, fd)
      else if fd.protoType.isTypeMessage && !fd.isMapField then
        checkNotRepeated("object", index)
        val sd.ScalaType.Message(d) = (fd.scalaType: @unchecked)
        MessageReader(d, false)
      else unexpectedType("object", index)

    override def visitArray(
        length: Int,
        index: Int
    ): ArrVisitor[sd.PValue, sd.PValue] =
      if fd.isMapField && formatMapEntriesAsKeyValuePairs then
        RepeatedReader(length, fd, true)
      else if fd.isRepeated && !fd.isMapField then
        RepeatedReader(length, fd, false)
      else unexpectedType("array", index)

  end FieldVisitor

  class KvOnlyVisitor(fd: sd.FieldDescriptor)
      extends SimpleVisitor[sd.PValue, sd.PValue]:
    override def expectedMsg: String = "expected object"
    override def visitObject(
        length: Int,
        jsonableKeys: Boolean,
        index: Int
    ): ObjVisitor[sd.PValue, sd.PValue] =
      val sd.ScalaType.Message(d) = (fd.scalaType: @unchecked)
      MessageReader(d, true)

  class RepeatedReader(sizeHint: Int, fd: sd.FieldDescriptor, kvOnly: Boolean)
      extends ArrVisitor[sd.PValue, sd.PValue]:
    private val buffer = collection.mutable.ArrayBuffer.empty[sd.PValue]
    buffer.sizeHint(sizeHint)

    val fv =
      if kvOnly then KvOnlyVisitor(fd) else FieldVisitor(fd, inArray = true)

    override def subVisitor: Visitor[?, ?] = fv

    override def visitValue(v: sd.PValue, index: Int): Unit =
      buffer += v

    override def visitEnd(index: Int): sd.PValue =
      sd.PRepeated(buffer.toVector)

  // special reader which reads JSON objects as protobuf maps
  class MapReader(sizeHint: Int, fd: sd.FieldDescriptor)
      extends ObjVisitor[sd.PValue, sd.PValue]:
    val mapEntryDescriptor =
      fd.scalaType.asInstanceOf[sd.ScalaType.Message].descriptor
    val keyDescriptor = mapEntryDescriptor.findFieldByNumber(1).get
    val valueDescriptor = mapEntryDescriptor.findFieldByNumber(2).get

    private var keyIndex: Int = -1
    private var key: sd.PValue = null

    val fv = FieldVisitor(valueDescriptor)

    override def visitKey(index: Int): Visitor[?, ?] =
      keyIndex = index
      KeyVisitor

    override def visitKeyValue(v: Any): Unit =
      val keyString = v.asInstanceOf[String]
      val keyValueOpt = keyDescriptor.scalaType match
        case sd.ScalaType.Boolean =>
          keyString.toBooleanOption.map(x => sd.PBoolean(x))
        case sd.ScalaType.Double =>
          keyString.toDoubleOption.map(x => sd.PDouble(x))
        case sd.ScalaType.Float =>
          keyString.toFloatOption.map(x => sd.PFloat(x))
        case sd.ScalaType.Int =>
          keyString.toIntOption.map(x => sd.PInt(x))
        case sd.ScalaType.Long =>
          keyString.toLongOption.map(x => sd.PLong(x))
        case sd.ScalaType.String =>
          Some(sd.PString(keyString))
        case _ =>
          throw JsonReadException("illegal key type", keyIndex)
      keyValueOpt match
        case None =>
          throw JsonReadException(
            s"cannot parse key '$keyString' of map field '${fd.fullName}' as a ${keyDescriptor.scalaType}",
            keyIndex
          )
        case Some(pvalue) => key = pvalue

    override def subVisitor: Visitor[?, ?] = fv

    private val entries = collection.mutable.ArrayBuffer.empty[sd.PMessage]
    entries.sizeHint(sizeHint)
    override def visitValue(v: sd.PValue, index: Int): Unit =
      val entry = sd.PMessage(
        Map(
          keyDescriptor -> key,
          valueDescriptor -> v
        )
      )
      entries += entry

    override def visitEnd(index: Int): sd.PValue =
      sd.PRepeated(entries.toVector)

  end MapReader

end JsonFormat
