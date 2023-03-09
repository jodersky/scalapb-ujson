// Partially copied from the scalapb-json4s project.
//
// https://github.com/scalapb/scalapb-json4s/blob/master/src/main/scala/scalapb/json4s/Durations.scala
//
// Released under the Apache 2.0 License.

package scalapb.ujson

import com.google.protobuf.duration.Duration
import com.google.protobuf.timestamp.Timestamp
import java.text.ParseException

object TimeUtils:

  def writeTimestamp(ts: Timestamp): String =
    java.time.Instant.ofEpochSecond(ts.seconds, ts.nanos).toString

  def parseTimestamp(value: String): Timestamp =
    val instant = java.time.Instant.parse(value)
    Timestamp(instant.getEpochSecond(), instant.getNano())

  def formatNanos(nanos: Int): String = {
    // Determine whether to use 3, 6, or 9 digits for the nano part.
    if (nanos % 1_000_000 == 0) {
      "%1$03d".format(nanos / 1_000_000)
    } else if (nanos % 1_000 == 0) {
      "%1$06d".format(nanos / 1_000)
    } else {
      "%1$09d".format(nanos)
    }
  }

  def writeDuration(d: Duration): String =
    val r = StringBuilder()
    if d.seconds < 0 || d.nanos < 0 then r.append("-")

    r.append(d.seconds.abs)
    if d.nanos != 0 then
      r.append(".")
      r.append(formatNanos(d.nanos))

    r.append("s")
    r.result()

  def parseDuration(value: String) =
    if !value.endsWith("s") then
      throw ParseException("Invalid duration string: " + value, 0)

    val (negative, number) =
      if value.startsWith("-") then (true, value.substring(1, value.length - 1))
      else (false, value.substring(0, value.length - 1))

    val pointPosition = number.indexOf('.')
    val (secondsStr, nanosStr) = if (pointPosition != -1) {
      (number.substring(0, pointPosition), number.substring(pointPosition + 1))
    } else {
      (number, "")
    }
    val seconds = secondsStr.toLong
    val nanos =
      if nanosStr.isEmpty then 0 else nanosStr.toInt

    if seconds < 0 then
      throw new ParseException("Invalid duration string: " + value, 0)

    Duration(
      seconds = if (negative) -seconds else seconds,
      nanos = if (negative) -nanos else nanos
    )
