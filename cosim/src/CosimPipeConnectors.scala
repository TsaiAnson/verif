package cosim

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chiseltest._
import verif._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{TransferSizes}
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

//    println("All Pipe Driver files exist")

    val in = new FileInputStream(pipe)

//    println("Pipe Driver streams opened")

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

//      println("All fence files exist")

      val req = new FileInputStream(fenceReqPipe)
//      println("Fence req connected")

      var resp_pending = false // Allow for req -> resp thread termination TODO: need to add this back in in some way
      var r: FenceProtos.FenceReq = null

      while(!terminate) {
        try {
          val r = FenceProtos.FenceReq.parseDelimitedFrom(req)
          if (r != null && r.getValid()) {
//            println(s"Recieved fence request: ${r.getNum()}")

            val resp = new FileOutputStream(fenceRespPipe)
//            println("Fence resp connected")

            while (io.busy.peek.litToBoolean || monitor.getMonitoredTransactions.size < r.getNum()) {
//              println(s"Busy is ${io.busy.peek.litToBoolean} and ${monitor.getMonitoredTransactions.size} seen")
              clock.step()
            } // step clock while busy

//            println("Sending fence resp")
            FenceProtos.FenceResp.newBuilder().setComplete(true).build().writeTo(resp)
            resp.close()
          }
        }
        catch {
          case e: IOException => println("IO Exception thrown in Fence Pipe")
        }
        clock.step()
      }

      req.close()

    }

    override def exit: Unit = {
      terminate = true
    }
}

case class TLCosimMemoryBufferState(txnBuffer: Seq[TLChannel])

class TLCosimMemoryInterface(tlaPipe: String, tldPipe: String, bundleParams: TLBundleParameters)(implicit p: Parameters) extends TLSlaveFunction[TLCosimMemoryBufferState] {
  // NOTE: Scala convention is to open inputs before outputs in matching pairs
  val tld_pipe = new FileInputStream(tldPipe)
//  println("TLD connected")

  override def response(tx: TLChannel, state: TLCosimMemoryBufferState): (Seq[TLChannel], TLCosimMemoryBufferState) = {
    tx match {
      case txA: TLBundleA =>
        val tla_buffer = state.txnBuffer :+ txA
        if (TLUtils.isCompleteTLTxn(tla_buffer, 16)) {
          tla_buffer.foreach(tla => {
            val tla_proto = VerifProtoBufUtils.BundleToProto(tla, TLProtos.TLA.newBuilder())
//            println("Monitored new TLA")
//            println(tla_proto)

            val tla_pipe = new FileOutputStream(tlaPipe)
//            println("TLA connected")

//            println("Writing TLA to pipe")
            tla_proto.writeTo(tla_pipe)
            tla_pipe.close()
          })

          var tld_buffer = Seq[TLChannel]()
          do {
            var tld_proto = com.verif.TLProtos.TLD.parseDelimitedFrom(tld_pipe)
            while(tld_proto == null) {
               tld_proto = com.verif.TLProtos.TLD.parseDelimitedFrom(tld_pipe)
            }
//            println("Recieved new TLD")
//            println(tld_proto)
            tld_buffer = tld_buffer :+ VerifProtoBufUtils.ProtoToBundle(tld_proto, VerifBundleUtils, new TLBundleD(bundleParams))
          } while (!TLUtils.isCompleteTLTxn(tld_buffer, 16))

          (tld_buffer, TLCosimMemoryBufferState(Seq()))
        } else {
          (Seq(), TLCosimMemoryBufferState(tla_buffer))
        }
      case _ => ???
    }
  }
}

class TLPipe(tlaPipe: String, tldPipe: String, clock: Clock, io: TLBundle)(implicit p: Parameters) extends AbstractCosimPipe { //TODO: Add driver and monitor
  override def run: Unit = {
    // Spin until all needed FIFOs are created
    while (!Files.exists(Paths.get(tlaPipe)) || !Files.exists(Paths.get(tldPipe))) {
      Thread.sleep(250)
    }
//    println("All TL files exist")

    val driver = new TLDriverSlave(clock, io, new TLCosimMemoryInterface(tlaPipe, tldPipe, io.params), TLCosimMemoryBufferState(Seq()))
  }

  override def exit: Unit = {}
}
