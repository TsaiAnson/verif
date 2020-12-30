package cosim

import chisel3._
import chisel3.util._
import java.net._
import java.io._
import scala.io._
import verif._

// A cosim driver server, recieves messages, translates them Bytes -> Message -> Bundle and pushes them into the driver
/*abstract class AbstractCosimDriverServer[I, S, D](driver: AbstractDriver[I, S, D],
                                                  service: () => io.grpc.BindableService, port: Int = 8008) {
  var server: io.grpc.Server = null

  def main(): Unit = {
    server = io.grpc.ServerBuilder
      .forPort(port)
      .addService(service()).build();

    Runtime.getRuntime().addShutdownHook(new Thread {
      override def run = {
        // Use stderr here since the logger may have been reset by its JVM shutdown hook.
        System.err.println("*** Shutting down gRPC server ***");
        try {
          AbstractCosimDriverServer.this.stop();
        } catch {
          case _: Throwable => System.err.println("*** Unexpected exception during gRPC server shutdow")
        }
        System.err.println("*** Server shut down ***");
      }
    })

    server.start();
    server.awaitTermination();
  }

  def stop(): Unit = {
    if (server != null) {
      server.shutdown().awaitTermination(30, java.util.concurrent.TimeUnit.SECONDS);
    }
  }
}

abstract class AbstractCosimClient[D <: com.google.protobuf.Message]() {

  def send(proto: D): Unit
}*/