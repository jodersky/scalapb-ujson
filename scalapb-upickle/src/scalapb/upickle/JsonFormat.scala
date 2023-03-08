package scalapb.upickle

import upickle.core.Visitor
import upickle.core.ArrVisitor
import upickle.core.ObjVisitor
import com.google.protobuf.ByteString
import scalapb.descriptors as sd
import scalapb.descriptors.PMessage

class JsonFormatException(msg: String, cause: Exception = null) extends Exception(msg, cause)

object JsonFormat:

  /** `this_is_snake_case => thisIsCamelCase` */
  def camelify(snake: String): String =
    val camel = new StringBuilder
    var prevIsUnder = false
    for c <- snake do
      if prevIsUnder then
        camel += c.toUpper
        prevIsUnder = false
      else if c == '_' then
        prevIsUnder = true
      else
        camel += c
        prevIsUnder = false
    camel.result()

  def defaultPrimitiveValue(fd: sd.FieldDescriptor): sd.PValue =
    import sd.ScalaType
    require(fd.isOptional)
    fd.scalaType match {
      case ScalaType.Int        => sd.PInt(0)
      case ScalaType.Long       => sd.PLong(0L)
      case ScalaType.Float      => sd.PFloat(0)
      case ScalaType.Double     => sd.PDouble(0)
      case ScalaType.Boolean    => sd.PBoolean(false)
      case ScalaType.String     => sd.PString("")
      case ScalaType.ByteString => sd.PByteString(ByteString.EMPTY)
      case ScalaType.Enum(ed)   => sd.PEnum(ed.values(0))
      case ScalaType.Message(_) => throw JsonFormatException(
        "no default value for a message; it is automatically constructed when writing a message"
      )
    }

class JsonFormat(
  val preserveProtoFieldNames: Boolean = true,
  val includeDefaultValueFields: Boolean = true,
  val formatEnumsAsNumber: Boolean = false,
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
    writeMessage(out, message.companion.scalaDescriptor.fields, message.toPMessage)

  def writeToJsonString(message: scalapb.GeneratedMessage, indent: Int = -1): String =
    val sb = new java.io.StringWriter
    write(ujson.Renderer(sb, indent), message)
    sb.toString()
  end writeToJsonString

  def writeToJson(message: scalapb.GeneratedMessage, indent: Int = -1): ujson.Value =
    write(ujson.Value, message)

  def writeMessage[V](
    out: Visitor[_, V],
    orderedFields: Seq[sd.FieldDescriptor], // PMessage doesn't have a field order, so we pass it in externally
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
          val sd.ScalaType.Message(md)  = (fd.scalaType: @unchecked)

          out.narrow.visitValue(
            writeMessage(
              out.subVisitor,
              md.fields,
              sd.PMessage(md.fields.map(f => f -> sd.PEmpty).toMap) // here PEmpty is not necessarily a missing *message* type
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
            val kv = x.asInstanceOf[PMessage]

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
              val sd.ScalaType.Message(md) = (valueDescriptor.scalaType: @unchecked)
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
              writeMessage(arrv.subVisitor, md.fields, x.asInstanceOf[sd.PMessage]),
              -1
            )
          out.narrow.visitValue(arrv.visitEnd(-1), -1)
        else
          val arrv = out.subVisitor.visitArray(xs.size, -1)
          for x <- xs do
            arrv.narrow.visitValue(
              writePrimitive(arrv.narrow.subVisitor, fd, x)
              ,
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
      if includeDefaultValueFields || !fd.isOptional || !fd.file.isProto3 || JsonFormat.defaultPrimitiveValue(fd) != other || fd.containingOneof.isDefined then
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
        if formatEnumsAsNumber then
          out.visitInt32(e.number, -1)
        else
          out.visitString(e.name, -1)

      case sd.PInt(v) if fd.protoType.isTypeUint32  =>
        out.visitInt64(unsignedInt(v), -1)
      case sd.PInt(v) if fd.protoType.isTypeFixed32 =>
        out.visitInt64(unsignedInt(v), -1)
      case sd.PInt(v)                               =>
        out.visitInt32(v, -1)
      case sd.PLong(v) if fd.protoType.isTypeUint64 =>
        out.visitUInt64(v, -1)
      case sd.PLong(v) if fd.protoType.isTypeFixed64 =>
        out.visitUInt64(v, -1)
      case sd.PLong(v) =>
        out.visitInt64(v, -1)
      case sd.PDouble(v)                            =>
        out.visitFloat64(v, -1)
      case sd.PFloat(v) =>
        out.visitFloat32(v, -1)
      case sd.PBoolean(v) =>
        if v then out.visitTrue(-1) else out.visitFalse(-1)
      case sd.PString(v)  =>
        out.visitString(v, -1)
      case sd.PByteString(v) =>
        val bytes = v.toByteArray()
        out.visitBinary(bytes, 0, bytes.length, -1)
      case _: sd.PMessage | sd.PRepeated(_) | sd.PEmpty =>
        throw new RuntimeException("should not happen")
    }

  // def writeDefault(
  //   out: Visitor[_, _],
  //   fd: sd.FieldDescriptor
  // ): Unit =
  //   import sd.ScalaType
  //   require(fd.isOptional)

  //   fd.scalaType match {
  //     case ScalaType.Int        =>
  //       out.visitInt32(0, -1)
  //     case ScalaType.Long       =>
  //       out.visitInt64(0, -1)
  //     case ScalaType.Float      =>
  //       out.visitFloat32(0, -1)
  //     case ScalaType.Double     =>
  //       out.visitFloat64(0, -1)
  //     case ScalaType.Boolean    =>
  //       out.visitFalse(-1)
  //     case ScalaType.String     =>
  //       out.visitString("", -1)
  //     case ScalaType.ByteString =>
  //       out.visitBinary(Array(), 0, 0, -1)
  //     case ScalaType.Enum(ed)   =>
  //       if formatEnumsAsNumber then
  //         out.visitInt32(0, -1)
  //       else
  //         out.visitString(
  //           ed.values(0).name,
  //           -1
  //         )
  //     case ScalaType.Message(desc) =>
  //       val fields = desc.fields
  //       val objv = out.visitObject(fields.size, true, -1)

  //       for fd <- fields do
  //         objv.visitKeyValue(objv.visitKey(-1).visitString(fd.name, -1), -1)
  //         objv.narrow.visitValue(
  //           writeDefault(
  //             objv.subVisitor,
  //             fd
  //           )
  //           ,
  //           -1
  //         )
  //       objv.visitEnd(-1)

  //   }

