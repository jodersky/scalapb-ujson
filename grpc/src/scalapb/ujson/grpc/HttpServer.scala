package scalapb.ujson.grpc

import io.undertow.server.HttpServerExchange

class HttpServer(
  services: Iterable[io.grpc.ServerServiceDefinition],
  host: String,
  port: Int
):
  private val undertow = io.undertow.Undertow.builder()
    .addHttpListener(port, host)
    .setHandler(
      io.undertow.server.handlers.BlockingHandler(
        DefaultHandler()
      )
    )
    .build()

  val endpoints = services.flatMap(s => Endpoint.generate(s))

  private class DefaultHandler extends io.undertow.server.HttpHandler:
    override def handleRequest(exchange: HttpServerExchange): Unit =
      endpoints.head.invoke(exchange)

  def start(): Unit = undertow.start()
  def stop(): Unit = undertow.stop()

object HttpServer:
  def apply(server: io.grpc.Server, host: String, port: Int): HttpServer =
    import scala.jdk.CollectionConverters.*
    new HttpServer(server.getServices().asScala, host, port)
