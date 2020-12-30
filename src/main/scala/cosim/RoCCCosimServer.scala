package cosim

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.{RoCCCommand}
import java.net._
import java.io._
import scala.io._
import verif._

// A cosim driver server, recieves messages, translates them Bytes -> Message -> Bundle and pushes them into the driver
/*class RoCCCommandCosimDriverServer (driver: DecoupledDriver[RoCCCommand],
                                    bundleBuilder: (com.google.protobuf.Message) => RoCCCommand,
                                    port: Int = 8008)
  extends AbstractCosimDriverServer[DecoupledIO[RoCCCommand], DecoupledTX[RoCCCommand], RoCCCommand] (driver, () => new RoCCCommandChannelImpl(driver, bundleBuilder), port) {

}

class RoCCCommandChannelImpl(driver: DecoupledDriver[RoCCCommand],
                                 bundleBuilder: (com.google.protobuf.Message) => RoCCCommand)
  extends com.verif.RoCCCommandChannelGrpc.RoCCCommandChannelImplBase {

  override def sendRoCCCommand(request: com.verif.RoCCProtos.RoCCCommand,
                               responseObserver: io.grpc.stub.StreamObserver[com.verif.RoCCProtos.RoCCCommandAck]): Unit = {
    var exit = 0
    try {
      println("Server recieved proto, attempting to push into driver")
      driver.push(driver.convertRawDataToStorage(bundleBuilder(request)))
    } catch {
      case _: Throwable => exit = -1
    }

    var response = com.verif.RoCCProtos.RoCCCommandAck
              .newBuilder()
              .setAck(exit)
              .build()

    responseObserver.onNext(response);
    responseObserver.onCompleted();
  }
}

class RoCCCommandCosimClient(channel: io.grpc.Channel) extends AbstractCosimClient[com.verif.RoCCProtos.RoCCCommand]() {

  final val blockingStub = com.verif.RoCCCommandChannelGrpc.newBlockingStub(channel)

  override def send(proto: com.verif.RoCCProtos.RoCCCommand): Unit = {
    println("Trying to send proto")
    // See if we can avoid try-catch
    try {
      val response = blockingStub.sendRoCCCommand(proto)
      println(s"Ack: ${response.getAck()}")
    } catch {
      case _: Throwable => {
        println("Sending proto failed")
        return
      }
    }
  }

}*/