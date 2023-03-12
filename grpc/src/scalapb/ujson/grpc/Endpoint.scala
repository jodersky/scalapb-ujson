package scalapb.ujson.grpc

import io.undertow.server.HttpServerExchange
import io.grpc.ServerMethodDefinition
import com.google.api.Http
import io.grpc.ServerCall
import io.grpc.Metadata
import scalapb.GeneratedMessage
import io.grpc.MethodDescriptor
import io.grpc.Status
import scalapb.GeneratedMessageCompanion
import io.grpc.MethodDescriptor.Marshaller
import java.io.InputStream

case class Endpoint(
  path: String,
  method: String,
  invoke: HttpServerExchange => Unit
)

object Endpoint:
  val fmt = scalapb.ujson.JsonFormat()

  def generate(sd: io.grpc.ServerServiceDefinition): Seq[Endpoint] =
    val endpoints = collection.mutable.ListBuffer.empty[Endpoint]
    sd.getMethods().forEach{ methodDef =>

      val schema = methodDef.getMethodDescriptor().getSchemaDescriptor()
        .asInstanceOf[scalapb.grpc.ConcreteProtoMethodDescriptorSupplier[_, _]]

      endpoints += generateMethod(
        methodDef.asInstanceOf[ServerMethodDefinition[scalapb.GeneratedMessage, scalapb.GeneratedMessage]],
        schema.inputCompanion.asInstanceOf[scalapb.GeneratedMessageCompanion[scalapb.GeneratedMessage]],
        schema.outputCompanion.asInstanceOf[scalapb.GeneratedMessageCompanion[scalapb.GeneratedMessage]]
      )
    }
    endpoints.result()

    // sd.getMethods().forEach()
    // val m = svc.serviceCompanion.scalaDescriptor.methods.head

    // val rule = com.google.api.annotations.AnnotationsProto.http.get(m.getOptions)
    // rule.get



  def generateMethod(
    method: ServerMethodDefinition[scalapb.GeneratedMessage, scalapb.GeneratedMessage],
    // rule: com.google.api.http.HttpRule,
    inputCompanion: scalapb.GeneratedMessageCompanion[scalapb.GeneratedMessage],
    outputCompanion: scalapb.GeneratedMessageCompanion[scalapb.GeneratedMessage]
  ) =

    val invoke = (exchange: HttpServerExchange) =>
      val input = fmt.read(exchange.getInputStream)(using inputCompanion)

      val latch = java.util.concurrent.CountDownLatch(1)
      val listener = method.getServerCallHandler().startCall(
        new ServerCall[scalapb.GeneratedMessage, scalapb.GeneratedMessage]:
          override def sendMessage(message: GeneratedMessage): Unit =
            println("yo")
            fmt.writeToOutputStream(message, exchange.getOutputStream())
            exchange.getOutputStream().flush()

          override def close(status: Status, trailers: Metadata): Unit =
            println("status: " + status)
            println("trailers: " + trailers)
            latch.countDown()
            // exchange.getOutputStream().close()

          override def getMethodDescriptor(): MethodDescriptor[GeneratedMessage, GeneratedMessage] =
            method.getMethodDescriptor()

          override def isCancelled(): Boolean = false

          override def request(numMessages: Int): Unit = ()

          override def sendHeaders(headers: Metadata): Unit = ()
        ,
        Metadata()
      )


      listener.onMessage(input)
      listener.onHalfClose()
      latch.await()


    Endpoint("/foo", "get", invoke)


  def jsonMarshaller[A <: GeneratedMessage](using c: GeneratedMessageCompanion[A]) =
    new Marshaller[A]:
      override def stream(value: A): InputStream =
        val bytes = fmt.writeToByteArray(value)
        java.io.ByteArrayInputStream(bytes)

      override def parse(stream: InputStream): A =
        fmt.read[A](stream)

  def mapMethod(
    method: ServerMethodDefinition[scalapb.GeneratedMessage, scalapb.GeneratedMessage],
    inputCompanion: scalapb.GeneratedMessageCompanion[scalapb.GeneratedMessage],
    outputCompanion: scalapb.GeneratedMessageCompanion[scalapb.GeneratedMessage]
  ) =
    val newMethod = MethodDescriptor
      .newBuilder[scalapb.GeneratedMessage, scalapb.GeneratedMessage]()
      .setFullMethodName("a/b")
      .setRequestMarshaller(jsonMarshaller[scalapb.GeneratedMessage](using inputCompanion))
      .setResponseMarshaller(jsonMarshaller[scalapb.GeneratedMessage](using outputCompanion))
      .setType(method.getMethodDescriptor().getType())
      .setSchemaDescriptor(method.getMethodDescriptor().getSchemaDescriptor())
      .build()





    ???
