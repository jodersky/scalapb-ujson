package scalapb.upickle2

import upickle.core.Visitor
import upickle.core.ArrVisitor
import upickle.core.ObjVisitor
import com.google.protobuf.ByteString
import scalapb.descriptors as sd

class JsonFormatException(msg: String, cause: Exception)
  extends Exception(msg, cause) {
  def this(msg: String) = this(msg, null)
}

object JsonFormat:

  def snakeCaseToCamelCase(
      name: String,
      upperInitial: Boolean = false
  ): String = {
    val b = new StringBuilder()
    @annotation.tailrec
    def inner(name: String, index: Int, capNext: Boolean): Unit =
      if (name.nonEmpty) {
        val (r, capNext2) = name.head match {
          case c if c.isLower => (Some(if (capNext) c.toUpper else c), false)
          case c if c.isUpper =>
            // force first letter to lower unless forced to capitalize it.
            (Some(if (index == 0 && !capNext) c.toLower else c), false)
          case c if c.isDigit => (Some(c), true)
          case _              => (None, true)
        }
        r.foreach(b.append)
        inner(name.tail, index + 1, capNext2)
      }
    inner(name, 0, upperInitial)
    b.toString
  }

  def jsonName(fd: sd.FieldDescriptor): String = {
    // protoc<3 doesn't know about json_name, so we fill it in if it's not populated.
    fd.asProto.jsonName
      .getOrElse(snakeCaseToCamelCase(fd.asProto.getName))
  }

  def defaultValue(fd: sd.FieldDescriptor): sd.PValue = {
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
      case ScalaType.Message(_) =>
        throw new RuntimeException("No default value for message")
    }
  }


class JsonFormat(
  val preserveProtoFieldNames: Boolean = true,
  val includeDefaultValueFields: Boolean = true,
  val formattingEnumsAsNumber: Boolean = false,
  val formattingMapEntriesAsKeyValuePairs: Boolean = false
):

  def write[M <: scalapb.GeneratedMessage, V](
    out: Visitor[_, V],
    message: M
  ): V =
    val descriptor = message.companion.scalaDescriptor
    val objVisitor = out.visitObject(
      length = descriptor.fields.size,
      jsonableKeys = true, // no idea what this does,
      -1
    )

    descriptor.fields.foreach{ f =>
      val name =
        if preserveProtoFieldNames then f.name
        else JsonFormat.jsonName(f)

      if f.protoType.isTypeMessage then
        writeMessageField(objVisitor, f, name, message.getFieldByNumber(f.number))
      else
        writeNonMessageField(objVisitor, f, name, message.getField(f))
        // serializeNonMessageField(f, name, m.getField(f), b)
    }
    objVisitor.visitEnd(-1)

  private def writeMessageField[T](
    out: ObjVisitor[T, _],
    fd: sd.FieldDescriptor,
    name: String,
    value: Any
  ): Unit = value match
    case null =>
    case Nil =>
      if includeDefaultValueFields then
        out.visitKeyValue(out.visitKey(-1).visitString(name, -1))
        if fd.isMapField && !formattingMapEntriesAsKeyValuePairs then
          val arr = out.subVisitor.visitArray(0, -1).visitEnd(-1)
          out.narrow.visitValue(arr, -1)
        else
          val obj = out.subVisitor.visitObject(0, false, -1).visitEnd(-1)
          out.narrow.visitValue(obj, -1)
    case xs: Iterable[scalapb.GeneratedMessage] @unchecked =>
      if fd.isMapField && !formattingMapEntriesAsKeyValuePairs then
        val mapEntryDescriptor =
          fd.scalaType.asInstanceOf[sd.ScalaType.Message].descriptor
        val keyDescriptor = mapEntryDescriptor.findFieldByNumber(1).get
        val valueDescriptor = mapEntryDescriptor.findFieldByNumber(2).get

        out.visitKeyValue(out.visitKey(-1).visitString(name, -1))
        val objv = out.subVisitor.visitObject(xs.size, true, -1)

        for x <- xs do
          val key = x.getField(keyDescriptor) match {
            case sd.PBoolean(v) => v.toString
            case sd.PDouble(v)  => v.toString
            case sd.PFloat(v)   => v.toString
            case sd.PInt(v)     => v.toString
            case sd.PLong(v)    => v.toString
            case sd.PString(v)  => v
            case v =>
              throw new JsonFormatException(s"Unexpected value for key: $v")
          }
          objv.visitKeyValue(objv.visitKey(-1).visitString(key, -1))

          if valueDescriptor.protoType.isTypeMessage then
            write(
              objv.narrow.subVisitor,
              x.getFieldByNumber(valueDescriptor.number).asInstanceOf[scalapb.GeneratedMessage]
            )
          else
            writeSingleValue(
              objv.narrow.subVisitor,
              valueDescriptor,
              x.getField(valueDescriptor)
            )
          out.narrow.visitValue(objv.visitEnd(-1), -1)

      else
        out.visitKeyValue(out.visitKey(-1).visitString(name, -1))
        val arrv = out.subVisitor.visitArray(xs.size, -1)
        for x <- xs do
          write(
            arrv.narrow.subVisitor,
            x
          )
        out.narrow.visitValue(arrv.visitEnd(-1), -1)

    case msg: scalapb.GeneratedMessage =>
      out.visitKeyValue(out.visitKey(-1).visitString(name, -1))
      out.narrow.visitValue(
        write(out.subVisitor, msg),
        -1
      )
    case v =>
      throw new JsonFormatException(v.toString)

  private def writeNonMessageField[T](
    out: ObjVisitor[T, _],
    fd: sd.FieldDescriptor,
    name: String,
    value: sd.PValue
  ): Unit = value match
    case sd.PEmpty =>
      if includeDefaultValueFields && fd.containingOneof.isEmpty then
        out.visitKeyValue(out.visitKey(-1).visitString(name, -1))

        out.narrow.visitValue(
          writeSingleValue(
            out.subVisitor,
            fd,
            JsonFormat.defaultValue(fd)
          ),
          -1
        )

    case sd.PRepeated(xs) =>
      if xs.nonEmpty || includeDefaultValueFields then
        out.visitKeyValue(out.visitKey(-1).visitString(name, -1))

        val arrv = out.subVisitor.visitArray(xs.size, -1).narrow
        for x <- xs do
          arrv.visitValue(
            writeSingleValue(
              arrv.subVisitor,
              fd,
              x
            ),
            -1
          )
        out.narrow.visitValue(
          arrv.visitEnd(-1),
          -1
        )

    case v =>
      if includeDefaultValueFields || !fd.isOptional || !fd.file.isProto3 || v != JsonFormat.defaultValue(fd) || fd.containingOneof.isDefined then
        out.visitKeyValue(out.visitKey(-1).visitString(name, -1))
        out.narrow.visitValue(
          writeSingleValue(out.subVisitor, fd, v),
          -1
        )

  private inline def unsignedInt(n: Int): Long = n & 0x00000000ffffffffL
  // private inline def unsignedLong(n: Long): BigInt =
  //   if (n < 0) BigInt(n & 0x7fffffffffffffffL).setBit(63) else BigInt(n)

  def writeSingleValue[V](
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
        if formattingEnumsAsNumber then
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
        throw new RuntimeException("Should not happen")
    }


  // class Printer[A] extends ujson.JsVisitor[A, _]:
  //   override def visitJsonableObject(length: Int, index: Int): ObjVisitor[A, Printer#J] = ???

  //   override def visitString(s: CharSequence, index: Int): J = ???

  //   override def visitNull(index: Int): J = ???

  //   override def visitFalse(index: Int): J = ???

  //   override def visitTrue(index: Int): J = ???

  //   override def visitArray(length: Int, index: Int): ArrVisitor[A, Any] = ???

  //   override def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): J = ???

  // def printer[A <: scalapb.GeneratedMessage] =


  //   ???
