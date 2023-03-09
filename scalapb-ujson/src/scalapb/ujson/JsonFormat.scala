package scalapb.ujson

import com.google.protobuf.duration.Duration
import com.google.protobuf.field_mask.FieldMask
import com.google.protobuf.timestamp.Timestamp
import com.google.protobuf.wrappers
import scalapb.descriptors as sd
import scalapb.FieldMaskUtil
import scalapb.GeneratedMessage
import ujson.Transformer
import upickle.core.ArrVisitor
import upickle.core.ObjVisitor
import upickle.core.Visitor

class JsonFormatException(msg: String, cause: Throwable = null)
    extends Exception(msg, cause)

// something went wrong reading JSON as a protobuf message
class JsonReadException(
    val message: String,
    val position: Int, // position in the JSON input
    cause: Throwable = null
) extends JsonFormatException(s"$message (position: $position)", cause)

object JsonFormatUtils:

  // descriptors of well-known messages
  final val TimestampDescriptor = Timestamp.scalaDescriptor
  final val DurationDescriptor = Duration.scalaDescriptor
  final val FieldMaskDescriptor = FieldMask.scalaDescriptor

  final val DoubleValueDescriptor = wrappers.DoubleValue.scalaDescriptor
  final val FloatValueDescriptor = wrappers.FloatValue.scalaDescriptor
  final val Int32ValueDescriptor = wrappers.Int32Value.scalaDescriptor
  final val Int64ValueDescriptor = wrappers.Int64Value.scalaDescriptor
  final val UInt32ValueDescriptor = wrappers.UInt32Value.scalaDescriptor
  final val UInt64ValueDescriptor = wrappers.UInt64Value.scalaDescriptor
  final val BoolValueDescriptor = wrappers.BoolValue.scalaDescriptor
  final val BytesValueDescriptor = wrappers.BytesValue.scalaDescriptor
  final val StringValueDescriptor = wrappers.StringValue.scalaDescriptor

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

/** The default instance. */
object JsonFormat extends JsonFormat(true, true, false)

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
  *   Default false. Use enum values' numbers instead of their names.
  */
class JsonFormat(
    val preserveProtoFieldNames: Boolean = true,
    val includeDefaultValueFields: Boolean = true,
    val formatEnumsAsNumbers: Boolean = false
):

  private def jsonName(fd: sd.FieldDescriptor): String =
    if preserveProtoFieldNames then fd.asProto.getName
    else
      // protoc<3 doesn't know about json_name, so we fill it in if it's not populated.
      fd.asProto.jsonName
        .getOrElse(JsonFormatUtils.camelify(fd.asProto.getName))

  def write(
      message: scalapb.GeneratedMessage,
      indent: Int = -1,
      escapeUnicode: Boolean = false
  ): String =
    val writer = java.io.StringWriter()
    writeTo(message, writer, indent, escapeUnicode)
    writer.toString

  def writeTo(
      message: scalapb.GeneratedMessage,
      out: java.io.Writer,
      indent: Int = -1,
      escapeUnicode: Boolean = false
  ): Unit =
    Writer.transform(message, ujson.Renderer(out, indent, escapeUnicode))

  def writeToOutputStream(
      message: scalapb.GeneratedMessage,
      out: java.io.OutputStream,
      indent: Int = -1,
      escapeUnicode: Boolean = false
  ): Unit =
    Writer.transform(
      message,
      ujson.BaseByteRenderer(out, indent, escapeUnicode)
    )

  def writeToByteArray(
      message: scalapb.GeneratedMessage,
      indent: Int = -1,
      escapeUnicode: Boolean = false
  ): Array[Byte] =
    val baos = java.io.ByteArrayOutputStream()
    writeToOutputStream(message, baos, indent, escapeUnicode)
    baos.toByteArray

  def writeToJson(
      message: scalapb.GeneratedMessage,
      indent: Int = -1,
      escapeUnicode: Boolean = false
  ): ujson.Value =
    Writer.transform(message, ujson.Value)

  object Writer extends Transformer[scalapb.GeneratedMessage]:
    override def transform[T](j: GeneratedMessage, f: Visitor[?, T]): T =
      writeMessage(f, j.companion.scalaDescriptor, j.toPMessage)

  private def writeMessage[V](
      out: Visitor[_, V],
      descriptor: sd.Descriptor,
      message: sd.PMessage
  ): V =
    // PMessage doesn't have a field order, so we use the descriptor to look them up
    val orderedFields: Seq[sd.FieldDescriptor] = descriptor.fields
    val fields = message.value

    descriptor match
      case JsonFormatUtils.TimestampDescriptor =>
        // PEmpty in a non-message field means that we're recursively completing with default values
        (fields(descriptor.findFieldByNumber(1).get): @unchecked) match
          case sd.PEmpty => out.visitString("1970-01-01T00:00:00Z", -1)
          case seconds: sd.PLong =>
            val nanos =
              fields(descriptor.findFieldByNumber(2).get).asInstanceOf[sd.PInt]

            // TODO: not ideal that we need to rebuild a Scala class instance from a PValue
            val str =
              TimeUtils.writeTimestamp(Timestamp(seconds.value, nanos.value))
            out.visitString(str, -1)

      case JsonFormatUtils.DurationDescriptor =>
        (fields(descriptor.findFieldByNumber(1).get): @unchecked) match
          case sd.PEmpty => out.visitString("0s", -1)
          case seconds: sd.PLong =>
            val nanos =
              fields(descriptor.findFieldByNumber(2).get).asInstanceOf[sd.PInt]

            // TODO: not ideal that we need to rebuild a Scala class instance from a PValue
            val str =
              TimeUtils.writeDuration(Duration(seconds.value, nanos.value))
            out.visitString(str, -1)

      case JsonFormatUtils.FieldMaskDescriptor =>
        (fields(descriptor.findFieldByNumber(1).get): @unchecked) match
          case sd.PEmpty           => out.visitString("", -1)
          case paths: sd.PRepeated =>
            // TODO: not ideal that we need to rebuild a Scala class instance from a PValue
            val str = FieldMaskUtil.toJsonString(
              FieldMask(paths.value.map(_.asInstanceOf[sd.PString].value))
            )
            out.visitString(str, -1)

      case JsonFormatUtils.DoubleValueDescriptor |
          JsonFormatUtils.FloatValueDescriptor |
          JsonFormatUtils.Int32ValueDescriptor |
          JsonFormatUtils.Int64ValueDescriptor |
          JsonFormatUtils.UInt32ValueDescriptor |
          JsonFormatUtils.UInt64ValueDescriptor |
          JsonFormatUtils.BoolValueDescriptor |
          JsonFormatUtils.BytesValueDescriptor |
          JsonFormatUtils.StringValueDescriptor =>
        val fd = descriptor.findFieldByNumber(1).get
        val pv = fields(fd)

        if pv != sd.PEmpty then
          writePrimitive(
            out,
            fd,
            fields(fd)
          )
        else out.visitNull(-1)

      case _ =>
        val objVisitor = out.visitObject(
          length = fields.size,
          jsonableKeys = true, // no idea what this does,
          -1
        )

        for descriptor <- orderedFields do
          val value = fields(descriptor)
          writeField(objVisitor, descriptor, value)

        objVisitor.visitEnd(-1)
  end writeMessage

  private def writeField(
      out: ObjVisitor[_, _],
      fd: sd.FieldDescriptor,
      value: sd.PValue
  ): Unit = value match
    case sd.PEmpty =>
      if includeDefaultValueFields then
        out.visitKeyValue(out.visitKey(-1).visitString(jsonName(fd), -1))

        if fd.containingOneof.isDefined then
          out.narrow.visitValue(
            out.subVisitor.visitNull(-1),
            -1
          )
        else
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
              md,
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
              JsonFormatUtils.defaultPrimitiveValue(fd)
            ),
            -1
          )
    case sd.PRepeated(xs) =>
      if xs.nonEmpty || includeDefaultValueFields then
        out.visitKeyValue(out.visitKey(-1).visitString(jsonName(fd), -1))

        if fd.isMapField then
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
                  md,
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
                md,
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
          md,
          msg
        ),
        -1
      )

    case other =>
      if includeDefaultValueFields || !fd.isOptional || !fd.file.isProto3 || JsonFormatUtils
          .defaultPrimitiveValue(fd) != other || fd.containingOneof.isDefined
      then
        out.visitKeyValue(out.visitKey(-1).visitString(jsonName(fd), -1))
        out.narrow.visitValue(
          writePrimitive(out.subVisitor, fd, other),
          -1
        )

  private inline def unsignedInt(n: Int): Long = n & 0x00000000ffffffffL

  private def writePrimitive[V](
      out: Visitor[_, V],
      fd: sd.FieldDescriptor,
      value: sd.PValue
  ): V =
    value match {
      case sd.PEnum(e) =>
        if formatEnumsAsNumbers then out.visitInt32(e.number, -1)
        else out.visitString(e.name, -1)

      case sd.PInt(v)  => out.visitInt32(v, -1)
      case sd.PLong(v) => out.visitString(v.toString, -1)
      case sd.PDouble(v) =>
        if v == Double.NaN then out.visitString("NaN", -1)
        else if v == Double.PositiveInfinity then
          out.visitString("Infinity", -1)
        else if v == Double.NegativeInfinity then
          out.visitString("-Infinity", -1)
        else out.visitFloat64(v, -1)
      case sd.PFloat(v) =>
        if v == Float.NaN then out.visitString("NaN", -1)
        else if v == Float.PositiveInfinity then out.visitString("Infinity", -1)
        else if v == Float.NegativeInfinity then
          out.visitString("-Infinity", -1)
        else out.visitFloat32(v, -1)
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

  def read[A <: scalapb.GeneratedMessage](json: ujson.Readable)(using
      companion: scalapb.GeneratedMessageCompanion[A]
  ): A =
    val pmessage = json.transform(Reader(companion.scalaDescriptor))
    companion.messageReads.read(pmessage)
end JsonFormat
