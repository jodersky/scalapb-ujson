
class PingImpl() extends pingpong.PingerGrpc.Pinger:

  override def sendPing(request: pingpong.Ping): concurrent.Future[pingpong.Pong] =
    sys.error("nooo")
    concurrent.Future.successful(pingpong.Pong(request.payload))
    // Future.failed(io.grpc.Status.NOT_FOUND.asException())

@main def main() =
  val service = pingpong.PingerGrpc.bindService(
    PingImpl(),
    scala.concurrent.ExecutionContext.global
  )

  val server = io.grpc.ServerBuilder
    .forPort(10000)
    .addService(service)
    .build()

  val httpServer = scalapb.ujson.grpc.HttpServer(server, "localhost", 10001)
  httpServer.start()

  println("done")
  server.start()
  server.awaitTermination()
