package scalapb.ujson.grpc.core

import scalapb.ujson.google.api.http
import scala.concurrent.Future
import svc.Ping
import svc.Pong
import io.grpc.ServerServiceDefinition
import io.grpc.ServerMethodDefinition
import io.grpc.ServerCall
import io.grpc.Metadata
import io.grpc.Status
import io.grpc.MethodDescriptor
import io.grpc.ServerInterceptor
import io.grpc.ServerCall.Listener
import io.grpc.ServerCallHandler
import io.grpc.ServerInterceptors
// import org.checkerframework.checker.units.qual.h

import com.google.api.Http

class PingImpl() extends svc.PingerGrpc.Pinger:

  override def sendPing(request: Ping): Future[Pong] =
    Future.successful(Pong(request.payload))
    // Future.failed(io.grpc.Status.NOT_FOUND.asException())


// public class GrpcServerRequestInterceptor implements ServerInterceptor {

//   @Override
//   public <ReqT, RespT> ServerCall.Listener<ReqT> interceptCall(
//       ServerCall<ReqT, RespT> serverCall, Metadata metadata, ServerCallHandler<ReqT, RespT> next) {

//     log.info("Validating user token");
//     var userToken = metadata.get(Metadata.Key.of("JWT", Metadata.ASCII_STRING_MARSHALLER));
//     validateUserToken(userToken);
//     return next.startCall(serverCall, metadata);
//   }

//   private void validateUserToken(String userToken) {
//     // Logic to validate token
//   }
// }


val AuthHeader = Metadata.Key.of("auth-code", Metadata.ASCII_STRING_MARSHALLER)

class Checker extends ServerInterceptor:

  override def interceptCall[ReqT, RespT](call: ServerCall[ReqT, RespT], headers: Metadata, next: ServerCallHandler[ReqT, RespT]): Listener[ReqT] =
    println(">>> checking")

    headers.get(AuthHeader) match
      case null =>
        call.close(Status.UNAUTHENTICATED.withDescription("no auth-code header given"), Metadata())
        new ServerCall.Listener[ReqT]() {}
      case "1234" =>
        next.startCall(call, headers)
      case _ =>
        call.close(Status.UNAUTHENTICATED.withDescription("invalid auth-code"), Metadata())
        new ServerCall.Listener[ReqT]() {}

@main
def main(): Unit =
  val service = svc.PingerGrpc.bindService(
    PingImpl(),
    scala.concurrent.ExecutionContext.global
  )

  val c = Checker()

  val server = io.grpc.ServerBuilder
    .forPort(10000)
    .addService(ServerInterceptors.intercept(service, c))
    .build()

  server.start()

  server.getServices().get(0).getMethods().forEach{ methodDef =>
    val schema = methodDef.getMethodDescriptor().getSchemaDescriptor()
      .asInstanceOf[scalapb.grpc.ConcreteProtoMethodDescriptorSupplier[_, _]]


    schema.getMethodDescriptor
    println(">>>>>" + schema.inputCompanion)
  }


  val m = server.getServices().get(0).getMethod("Pinger/SendPing").asInstanceOf[ServerMethodDefinition[Ping, Pong]]

  val latch = java.util.concurrent.CountDownLatch(1)

  val dummycall = new ServerCall[Ping, Pong]:

    override def getMethodDescriptor(): MethodDescriptor[Ping, Pong] =
      m.getMethodDescriptor()

    override def sendHeaders(headers: Metadata): Unit =
      println("receiving response headers")
      println(headers)

    override def sendMessage(message: Pong): Unit =
      println("response: " + message)

    override def isCancelled(): Boolean = false

    override def request(numMessages: Int): Unit =
      println("requesting " + numMessages)
      latch.countDown()
      ()

    override def close(status: Status, trailers: Metadata): Unit =
      println("status: " + status)
      println("trailers: " + trailers)

  val reqheaders = Metadata()
  reqheaders.put(AuthHeader, "1234")

  val listener = m.getServerCallHandler()
    .startCall(dummycall, reqheaders)

  // latch.await()
  println("sending")
  listener.onMessage(Ping("hello, world"))
  listener.onHalfClose()


  // server.getServices().get(0).

  // _root_.io.grpc.stub.ServerCalls.asyncUnaryCall(new _root_.io.grpc.stub.ServerCalls.UnaryMethod[svc.Ping, svc.Pong] {
  //         override def invoke(request: svc.Ping, observer: _root_.io.grpc.stub.StreamObserver[svc.Pong]): _root_.scala.Unit =
  //           serviceImpl.sendPing(request).onComplete(scalapb.grpc.Grpc.completeObserver(observer))(
  //             executionContext)
  //       }))


  // m.getServerCallHandler()
  //   .startCall(
  //     new io.grpc.ServerCall[Ping, Pong] {},
  //     ???
  //   )
  //   .startCall(???, ???).
  // println(m.getMethodDescriptor().getFullMethodName())

  // println(service.getMethods().forEach(m => println(m.getMethodDescriptor().getFullMethodName())))

  server.awaitTermination()

// object Mapper:

//   val server = io.grpc.ServerBuilder
//     .forPort(10000)
//     .build()

//   server.getServices().get(0).getMethod("").getServerCallHandler().

//   // svc.Pinger.bindService(???, ???)
//   svc.PingerGrpc.bindService(???, ???)

//   ()
