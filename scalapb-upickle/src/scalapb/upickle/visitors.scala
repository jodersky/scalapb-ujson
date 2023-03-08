package scalapb.upickle

import scalapb.descriptors as sd
import upickle.core.Visitor
import upickle.core.ObjVisitor
import upickle.core.ArrVisitor

trait SimpleVisitor[-T, +V] extends Visitor[T, V]:
  def expectedMsg: String

  def visitNull(index: Int): V = null.asInstanceOf[V]
  def visitTrue(index: Int): V =  throw JsonReadException(expectedMsg + " got boolean", index)
  def visitFalse(index: Int): V = throw JsonReadException(expectedMsg + " got boolean", index)

  def visitString(s: CharSequence, index: Int): V = {
    throw JsonReadException(expectedMsg + " got string", index)
  }
  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): V = {
    throw JsonReadException(expectedMsg + " got number", index)
  }

  def visitObject(length: Int, jsonableKeys: Boolean, index: Int): ObjVisitor[T, V] = {
    throw JsonReadException(expectedMsg + " got object", index)
  }
  def visitArray(length: Int, index: Int): ArrVisitor[T, V] = {
    throw JsonReadException(expectedMsg + " got array", index)
  }

  def visitFloat64(d: Double, index: Int): V = throw JsonReadException(expectedMsg + " got float64", index)

  def visitFloat32(d: Float, index: Int): V = throw JsonReadException(expectedMsg + " got float32", index)

  def visitInt32(i: Int, index: Int): V = throw JsonReadException(expectedMsg + " got int32", index)

  def visitInt64(i: Long, index: Int): V = throw JsonReadException(expectedMsg + " got int64", index)

  def visitUInt64(i: Long, index: Int): V = throw JsonReadException(expectedMsg + " got uint64", index)

  def visitFloat64String(s: String, index: Int): V = throw JsonReadException(expectedMsg + " got float64 string", index)

  def visitChar(s: Char, index: Int): V = throw JsonReadException(expectedMsg + " got char", index)

  def visitBinary(bytes: Array[Byte], offset: Int, len: Int, index: Int): V = throw JsonReadException(expectedMsg + " got binary", index)

  def visitExt(tag: Byte, bytes: Array[Byte], offset: Int, len: Int, index: Int): V = throw JsonReadException(expectedMsg + " got ext", index)


object NoOpVisitor extends Visitor[sd.PValue, sd.PValue] {

  def visitArray(length: Int, index: Int) = new ArrVisitor[sd.PValue, sd.PValue] {
    def subVisitor = NoOpVisitor.this
    def visitValue(v: sd.PValue, index: Int): Unit = ()
    def visitEnd(index: Int): sd.PValue = sd.PEmpty
  }
  def visitObject(length: Int, jsonableKeys: Boolean, index: Int) = new ObjVisitor[sd.PValue, sd.PValue] {
    def subVisitor = NoOpVisitor.this
    def visitKey(index: Int) = NoOpVisitor
    def visitKeyValue(s: Any): Unit =()
    def visitValue(v: sd.PValue, index: Int): Unit = ()
    def visitEnd(index: Int): sd.PValue = sd.PEmpty
  }

  def visitNull(index: Int): sd.PValue = sd.PEmpty
  def visitFalse(index: Int): sd.PValue = sd.PEmpty
  def visitTrue(index: Int): sd.PValue = sd.PEmpty
  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): sd.PValue = sd.PEmpty
  def visitString(s: CharSequence, index: Int): sd.PValue = sd.PEmpty

  def visitFloat64(d: Double, index: Int) = sd.PEmpty

  def visitFloat32(d: Float, index: Int) = sd.PEmpty

  def visitInt8(i: Byte, index: Int) = sd.PEmpty
  def visitUInt8(i: Byte, index: Int) = sd.PEmpty

  def visitInt16(i: Short, index: Int) = sd.PEmpty
  def visitUInt16(i: Short, index: Int) = sd.PEmpty

  def visitInt32(i: Int, index: Int) = sd.PEmpty
  def visitUInt32(i: Int, index: Int) = sd.PEmpty

  def visitInt64(i: Long, index: Int) = sd.PEmpty
  def visitUInt64(i: Long, index: Int) = sd.PEmpty

  def visitFloat64String(s: String, index: Int) = sd.PEmpty

  def visitBinary(bytes: Array[Byte], offset: Int, len: Int, index: Int) = sd.PEmpty

  def visitFloat64StringParts(s: CharSequence, decIndex: Int, expIndex: Int) = sd.PEmpty

  def visitExt(tag: Byte, bytes: Array[Byte], offset: Int, len: Int, index: Int) = sd.PEmpty

  def visitChar(s: Char, index: Int) = sd.PEmpty
}

object KeyVisitor extends SimpleVisitor[_, String]:
  override def visitString(s: CharSequence, index: Int): String = s.toString()
  val expectedMsg = "expected JSON string for a field name"
