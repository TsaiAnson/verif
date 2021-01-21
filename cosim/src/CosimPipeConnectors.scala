package cosim

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chiseltest._
import verif._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.{RoCCCommand, RoCCIO}
import com.verif.RoCCProtos
import com.verif.FenceProtos
import java.io.{FileInputStream, FileOutputStream}
import java.nio.file.{Files, Paths}

class RoCCCommandCosimPipeDriver(pipe: String, clock: Clock, target: verif.DecoupledDriver[RoCCCommand])(implicit p: Parameters) extends
  AbstractCosimPipeDriver[DecoupledIO[RoCCCommand], DecoupledTX[RoCCCommand], RoCCProtos.RoCCCommand](pipe) {
    val driver = target

    val inputStreamToProto = (input: java.io.InputStream) => {
      com.verif.RoCCProtos.RoCCCommand.parseDelimitedFrom(input)
    }

    def pushIntoDriver(message: RoCCProtos.RoCCCommand): Unit = {
      driver.push(new DecoupledTX(VerifProtoBufUtils.ProtoToBundle(message, VerifBundleUtils, new RoCCCommand)))
    }
}

class FencePipe(fenceReqPipe: String, fenceRespPipe: String, clock: Clock, io: RoCCIO)(implicit p: Parameters) extends
  AbstractCosimPipe {
    @volatile private var terminate = false

    override def run: Unit = {
      // Spin until all needed FIFOs are created
      while (!Files.exists(Paths.get(fenceReqPipe)) || !Files.exists(Paths.get(fenceRespPipe))) {
        Thread.sleep(250)
      }

      println("All Fence files exist")

      val req = new FileInputStream(fenceReqPipe)
      val resp = new FileOutputStream(fenceRespPipe)

      println("Fence streams opened")

      while(!terminate) {
        val r = FenceProtos.FenceReq.parseDelimitedFrom(req)
        if (r != null) {
          println("Non-null fence req seen")
          println(r.getValid())
        }
        if (r != null && r.asInstanceOf[FenceProtos.FenceReq].getValid()) {
//          while(io.busy.peek().litToBoolean) {
//            println("Waiting for busy to go low")
//            clock.step()
//          } // step clock while busy
          println("Sending response")
          FenceProtos.FenceResp.newBuilder().setComplete(true).build().writeDelimitedTo(resp)
        }
      }
    }

    override def exit: Unit = {
      terminate = true
    }
}

class TLPipe(tlaPipe: String, tldPipe: String, clock: Clock) extends AbstractCosimPipe { //TODO: Add driver and monitor
  @volatile private var terminate = false

  override def run: Unit = {
    // Spin until all needed FIFOs are created
    while (!Files.exists(Paths.get(tlaPipe)) || !Files.exists(Paths.get(tldPipe))) {
      Thread.sleep(250)
    }

    println("All TL files exist")

    // NOTE: Scala convention is to open inputs before outputs in matching pairs
    val tld = new FileInputStream(tldPipe)
    val tla = new FileOutputStream(tlaPipe)

    println("TL streams opened")

    while(!terminate) {

    }
  }

  override def exit: Unit = {
    terminate = true
  }
}
