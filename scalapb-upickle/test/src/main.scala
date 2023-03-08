@main def main() =
  val m = protos.Dummy(
    42
  )
  val msg = m.toPMessage
  println(msg)

  // for (fd, _) <- msg.value do
  //   println(s"${fd.name} -> ${fd.isMapField}")

  for (fd, _) <- msg.value do
    println(s"${fd.name} -> ${fd.protoType.isTypeMessage}")

  println("---------")
  for f <- m.companion.scalaDescriptor.fields do
    println(m.getFieldByNumber(f.number))


  // import scalapb.descriptors.*

  // PMessage(
  //   Map(
  //     Dummy.number -> PInt(42),
  //     Dummy.str -> PString("hello world"),
  //     Dummy.data -> PRepeated(Vector()), Dummy.msg -> PEmpty))
