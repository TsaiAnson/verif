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
import com.verif.TLProtos
import java.io.{BufferedOutputStream, FileInputStream, FileOutputStream, IOException}
import java.nio.file.{Files, Paths}

abstract class AbstractCosimPipe extends Runnable {
  def run: Unit
  def exit: Unit
}

abstract class DecoupledCosimPipeDriver[I, S, D](pipe: String) extends AbstractCosimPipe {
  @volatile private var terminate = false

  val driver: AbstractDriver[I, S]
  val inputStreamToProto: (java.io.InputStream) => D

  def pushIntoDriver(message: D): Unit

  override def run: Unit = {
    // Spin until all needed FIFOs are created
    while (!Files.exists(Paths.get(pipe))) {
      Thread.sleep(250)
    }

    println("All Pipe Driver files exist")

    val in = new FileInputStream(pipe)

    println("Pipe Driver streams opened")

    while(!terminate) {
      val message = inputStreamToProto(in)
      if (message != null) {
        pushIntoDriver(message)
      }
    }

    in.close
  }

  override def exit: Unit = {
    terminate = true
  }
}

class RoCCCommandCosimPipeDriver(pipe: String, clock: Clock, io: DecoupledIO[RoCCCommand])(implicit p: Parameters) extends
  DecoupledCosimPipeDriver[DecoupledIO[RoCCCommand], DecoupledTX[RoCCCommand], RoCCProtos.RoCCCommand](pipe) {
    val driver = new DecoupledDriverMaster(clock, io)

    val inputStreamToProto = (input: java.io.InputStream) => {
      com.verif.RoCCProtos.RoCCCommand.parseDelimitedFrom(input)
    }

    def pushIntoDriver(message: RoCCProtos.RoCCCommand): Unit = {
      driver.push(new DecoupledTX(new RoCCCommand).tx(VerifProtoBufUtils.ProtoToBundle(message, VerifBundleUtils, new RoCCCommand)))
    }
}

class FencePipe(fenceReqPipe: String, fenceRespPipe: String, clock: Clock, io: RoCCIO)(implicit p: Parameters) extends
  AbstractCosimPipe {
    @volatile private var terminate = false

    val monitor = new DecoupledMonitor(clock, io.cmd)

    override def run: Unit = {
      // Spin until all needed FIFOs are created
      while (!Files.exists(Paths.get(fenceReqPipe)) || !Files.exists(Paths.get(fenceRespPipe))) {
        Thread.sleep(250)
      }

      println("All fence files exist")

      val req = new FileInputStream(fenceReqPipe)
      println("Fence req connected")

      var resp_pending = false // Allow for req -> resp thread termination TODO: need to add this back in in some way
      var r: FenceProtos.FenceReq = null

      while(!terminate) {
        try {
          val r = FenceProtos.FenceReq.parseDelimitedFrom(req)
          if (r != null && r.getValid()) {
            println(s"Recieved fence request: ${r.getNum()}")

            val resp = new FileOutputStream(fenceRespPipe)
            println("Fence resp connected")

            var i = 0; // 10 cycle timeout for testing
            while ((io.busy.peek.litToBoolean || monitor.getMonitoredTransactions.size <= r.getNum()) && i < 50) {
              println(s"Busy is ${io.busy.peek.litToBoolean} and ${monitor.getMonitoredTransactions.size} seen")
              if (io.busy.peek.litToBoolean) {
                i = i + 1
              }
              clock.step()
            } // step clock while busy

            println("Sending fence resp")
            FenceProtos.FenceResp.newBuilder().setComplete(true).build().writeTo(resp)
            resp.close()
          }
        }
        catch {
          case e: IOException => println("IO Exception thrown in Fence Pipe")
        }
        clock.step()
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
  println("TLD connected")
  var tla = new FileOutputStream(tlaPipe)
  println("TLA connected") // We leave TLA open to start and close + reopen after each sent message

  override def response(tx: TLChannel, state: Any): (Seq[TLChannel], Any) = {
    tx match {
      case txA: TLBundleA =>
        //val tla_bundle = VerifProtoBufUtils.ProtoToBundle(tla, VerifBundleUtils, new TLBundleA(VerifTestUtils.getVerifTLBundleParameters(16, 32, TransferSizes(1,64))))
        val tla_proto = VerifProtoBufUtils.BundleToProto(txA, TLProtos.TLA.newBuilder())
        println(tla_proto)

        println("Writing TLA to pipe")
        tla_proto.writeTo(tla)
        tla.close()

        tla = new FileOutputStream(tlaPipe)
        println("TLA connected")

        (Seq[TLBundleD](), null)
      case _ => ???
    }
  }
}

class TLPipe(tlaPipe: String, tldPipe: String, clock: Clock, io: TLBundle) extends AbstractCosimPipe { //TODO: Add driver and monitor
  override def run: Unit = {
    // Spin until all needed FIFOs are created
    while (!Files.exists(Paths.get(tlaPipe)) || !Files.exists(Paths.get(tldPipe))) {
      Thread.sleep(250)
    }
    println("All TL files exist")

    val driver = new TLDriverSlave(clock, io, new TLCosimMemoryInterface(tlaPipe, tldPipe), null)
  }

  override def exit: Unit = {}
}
