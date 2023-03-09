package scalapb.ujson

import upickle.core.Visitor
import upickle.core.ArrVisitor
import upickle.core.ObjVisitor
import scalapb.descriptors as sd
import scalapb.FieldMaskUtil

class Reader(md: sd.Descriptor) extends SimpleVisitor[sd.PValue, sd.PMessage]:
  override val expectedMsg: String = "expected JSON object"
  override def visitObject(
      length: Int,
      jsonableKeys: Boolean,
      index: Int
  ): ObjVisitor[sd.PValue, sd.PMessage] =
    MessageReader(md)

private class MessageReader(md: sd.Descriptor)
    extends ObjVisitor[sd.PValue, sd.PMessage]:
  private val parsedFields =
    collection.mutable.Map.empty[sd.FieldDescriptor, sd.PValue]

  private var key: String = null
  private var keyIndex: Int = -1
  private val fv = FieldVisitor(null)

  private val camelified = md.fields.map { field =>
    val name = field.asProto.jsonName
      .getOrElse(JsonFormat.camelify(field.asProto.getName))
    name -> field
  }.toMap

  override def visitKey(index: Int): Visitor[?, ?] =
    keyIndex = index
    KeyVisitor

  override def visitKeyValue(v: Any): Unit =
    key = v.asInstanceOf[String]
    md.findFieldByName(key).orElse(camelified.get(key)) match
      case Some(d) => fv.fd = d
      case None    => fv.fd = null

  override def subVisitor: Visitor[?, ?] =
    if fv.fd == null then NoOpVisitor else fv

  override def visitValue(v: sd.PValue, index: Int): Unit =
    if v != sd.PEmpty then parsedFields += fv.fd -> v

  override def visitEnd(index: Int): sd.PMessage =
    sd.PMessage(parsedFields.toMap)

end MessageReader

// PMessage already does error checking, but the messages aren't friendly
private class FieldVisitor(var fd: sd.FieldDescriptor, inArray: Boolean = false)
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
    else if pt.isTypeEnum then
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

    if fd.protoType.isTypeEnum then
      val sd.ScalaType.Enum(ed) = (fd.scalaType: @unchecked)
      ed.values.find(_.name == s.toString) match
        case None     => sd.PEmpty // ignore unknown value
        case Some(ev) => sd.PEnum(ev)
    else if fd.protoType.isTypeString then
      sd.PString(s.toString())
    else if fd.protoType.isTypeBytes then
      sd.PByteString(
        com.google.protobuf.ByteString.copyFrom(
          java.util.Base64.getDecoder().decode(s.toString)
        )
      )
    else if fd.protoType.isTypeMessage then
      val sd.ScalaType.Message(d) = (fd.scalaType: @unchecked)

      def specialParse(tpe: String)(action: => sd.PValue) =
        try
          action
        catch
          case t: Throwable =>
            throw JsonReadException(s"error for protobuf fiel '${fd.fullName}', parsing string as $tpe: ${t.getMessage}", index, t)

      d match
        case JsonFormat.TimestampDescriptor =>
          specialParse("timestamp") {
            Timestamps.parseTimestamp(s.toString).toPMessage
          }
        case JsonFormat.DurationDescriptor =>
          specialParse("duration") {
            Durations.parseDuration(s.toString).toPMessage
          }
        case JsonFormat.FieldMaskDescriptor =>
          specialParse("fieldmask") {
            FieldMaskUtil.fromJsonString(s.toString).toPMessage
          }
        case _ => unexpectedType("string", index)
    else unexpectedType("string", index)

  override def visitNull(index: Int) =
    sd.PEmpty // we treat null as an omitted field

  override def visitJsonableObject(
      length: Int,
      index: Int
  ): ObjVisitor[sd.PValue, sd.PValue] =
    if fd.isMapField then
      MapReader(length, fd)
    else if fd.protoType.isTypeMessage then
      checkNotRepeated("object", index)
      val sd.ScalaType.Message(d) = (fd.scalaType: @unchecked)
      MessageReader(d)
    else unexpectedType("object", index)

  override def visitArray(
      length: Int,
      index: Int
  ): ArrVisitor[sd.PValue, sd.PValue] =
    if fd.isRepeated && !fd.isMapField then
      RepeatedReader(length, fd)
    else unexpectedType("array", index)

end FieldVisitor

private class RepeatedReader(sizeHint: Int, fd: sd.FieldDescriptor)
    extends ArrVisitor[sd.PValue, sd.PValue]:
  private val buffer = collection.mutable.ArrayBuffer.empty[sd.PValue]
  buffer.sizeHint(sizeHint)

  val fv = FieldVisitor(fd, inArray = true)

  override def subVisitor: Visitor[?, ?] = fv

  override def visitValue(v: sd.PValue, index: Int): Unit =
    buffer += v

  override def visitEnd(index: Int): sd.PValue =
    sd.PRepeated(buffer.toVector)

// special reader which reads JSON objects as protobuf maps
private class MapReader(sizeHint: Int, fd: sd.FieldDescriptor)
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
