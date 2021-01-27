package cosim

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chiseltest._
import verif._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.{RoCCCommand, RoCCIO}
import freechips.rocketchip.tilelink.{TLBundle, TLBundleA, TLBundleD, TLBundleParameters, TLChannel}
import com.verif.RoCCProtos
import com.verif.FenceProtos
import java.io.{BufferedOutputStream, FileInputStream, FileOutputStream, IOException}
import java.nio.file.{Files, Paths}

class RoCCCommandCosimPipeDriver(pipe: String, clock: Clock, io: DecoupledIO[RoCCCommand])(implicit p: Parameters) extends
  AbstractCosimPipeDriver[DecoupledIO[RoCCCommand], DecoupledTX[RoCCCommand], RoCCProtos.RoCCCommand](pipe) {
    val driver = new DecoupledDriverMaster(clock, io)

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

      println("All fence files exist")

      val req = new FileInputStream(fenceReqPipe)
      println("Fence req connected")

      var resp_pending = false // Allow for req -> resp thread termination
      var r: FenceProtos.FenceReq = null

      while(!terminate) {
        try {
          if (resp_pending) {
            resp_pending = false
            val resp = new FileOutputStream(fenceRespPipe)
            println("Fence resp connected")

            println("Sending fence resp")
            //          while(io.busy.peek().litToBoolean) {
            //            println("Waiting for busy to go low")
            //            clock.step()
            //          } // step clock while busy
            FenceProtos.FenceResp.newBuilder().setComplete(true).build().writeTo(resp)
            resp.close()
          } else {
            r = FenceProtos.FenceReq.parseDelimitedFrom(req)
            if (r != null && r.getValid()) {
              println("Recieved fence request")
              resp_pending = true
            }
          }
        }
        catch {
          case e: IOException => println("IO Exception thrown in Fence Pipe")
        }
      }

      req.close
      //resp.close

    }

    override def exit: Unit = {
      terminate = true
    }
}

class TLCosimMemoryInterface(tlaPipe: String, tldPipe: String) extends TLSlaveFunction[Any] {
  // NOTE: Scala convention is to open inputs before outputs in matching pairs
  val tld = new FileInputStream(tldPipe)
  val tla = new FileOutputStream(tlaPipe)
  println("TL streams opened")

  override def response(tx: TLChannel, state: Any): (Seq[TLChannel], Any) = {
    return (Seq.empty[TLChannel], null)
  }
}

class TLPipe(tlaPipe: String, tldPipe: String, clock: Clock, io: TLBundle) extends AbstractCosimPipe { //TODO: Add driver and monitor
  @volatile private var terminate = false

  override def run: Unit = {
    // Spin until all needed FIFOs are created
    while (!Files.exists(Paths.get(tlaPipe)) || !Files.exists(Paths.get(tldPipe))) {
      Thread.sleep(250)
    }
    println("All TL files exist")

    val driver = new TLDriverSlave(clock, io, new TLCosimMemoryInterface(tlaPipe, tldPipe), null)

    while(!terminate) {
    }
  }

  override def exit: Unit = {
    terminate = true
  }
}
