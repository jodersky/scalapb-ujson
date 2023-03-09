package scalapb.ujson

import upickle.core.Visitor
import scalapb.descriptors as sd

trait MessageWriter:
  def writeMessage[B](visitor: Visitor[_, B], message: sd.PMessage): B

trait MessageReader extends SimpleVisitor[Any, sd.PMessage]:
  def tpe: String
  def expectedMsg: String = s"expected a $tpe"

class FormatRegistry():

  private val _specialWriters = collection.mutable.Map.empty[sd.Descriptor, MessageWriter]
  private val _specialReaders = collection.mutable.Map.empty[sd.Descriptor, MessageReader]

  class Writer[A <: scalapb.GeneratedMessage]
    (val write: [B] => (Visitor[_, B], A) => B)
    (using companion: scalapb.GeneratedMessageCompanion[A]) extends MessageWriter:

    // It's not great that we construct a Scala instance from the PMessage.
    // However, the alternative is fiddling with extracting data from PMessages
    // with descriptors manually, which basically amounts to creating a Scala type
    // and using that to extract data. Some benchmarks would be useful to assess
    // the performance differences.
    def writeMessage[B](visitor: Visitor[_, B], message: sd.PMessage): B =
      write(visitor, companion.messageReads.read(message))

  def register[A <: scalapb.GeneratedMessage](
    w: [B] => (Visitor[_, B], A) => B,
    r: MessageReader
  )(using c: scalapb.GeneratedMessageCompanion[A]) =
    _specialWriters += c.scalaDescriptor -> Writer[A](w)
    _specialReaders += c.scalaDescriptor -> r

  def specialWriters = _specialWriters.toMap
  def specialReaders = _specialReaders.toMap

  def primitiveWrapper[A <: scalapb.GeneratedMessage]
    (using c: scalapb.GeneratedMessageCompanion[A]) =

    val fd = c.scalaDescriptor.findFieldByNumber(1).get
    new MessageWriter:
      def writeMessage[B](visitor: Visitor[_, B], message: sd.PMessage): B =
        message.value(fd)
        ???



class DefaultRegistry() extends FormatRegistry:
  import com.google.protobuf.timestamp.Timestamp

  register[Timestamp](
    [B] => (visitor: Visitor[_, B], ts: Timestamp) =>
      visitor.visitString(Timestamps.writeTimestamp(ts), -1),
    new MessageReader:
      val tpe = "timestamp"
      override def visitString(s: CharSequence, index: Int): sd.PMessage =
        try
          Timestamps.parseTimestamp(s.toString()).toPMessage
        catch
          case t: Throwable =>
            throw JsonReadException(s"cannot parse timestamp: ${t.getMessage}", index, t)
  )

object DefaultRegistry extends DefaultRegistry()
