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

trait AbstractCosimPipe

abstract class DecoupledPipeDriver[T <: Data, D](pipeName: String, clock: Clock)(implicit p: Parameters, cosimTestDetails: CosimTestDetails) extends AbstractCosimPipe {

  val driver: DecoupledDriverMaster[T]
  val inputStreamToProto: (java.io.InputStream) => D

  def pushIntoDriver(message: D): Unit

  val pipe = s"${cosimTestDetails.testPath.get}/cosim_run_dir/${pipeName}"

  fork {
    while (!Files.exists(Paths.get(pipe))) {
      Thread.sleep(250)
    }

    val in = new FileInputStream(pipe)

    clock.step()

    while (true) {
     if (in.available != 0) {
       val message = inputStreamToProto(in)
        if (message != null) {
          pushIntoDriver(message)
        }
      }
      clock.step()
    }
  }
}

class RoCCCommandPipeDriver(pipeName: String, clock: Clock, io: DecoupledIO[RoCCCommand])(implicit p: Parameters, cosimTestDetails: CosimTestDetails) extends DecoupledPipeDriver[RoCCCommand, RoCCProtos.RoCCCommand](pipeName, clock) {

  val driver = new DecoupledDriverMaster(clock, io)
  val inputStreamToProto = (in: java.io.InputStream) => RoCCProtos.RoCCCommand.parseDelimitedFrom(in)

  override def pushIntoDriver(message: RoCCProtos.RoCCCommand): Unit = {
    driver.push(new DecoupledTX(new RoCCCommand).tx(VerifProtoBufUtils.ProtoToBundle(message, VerifBundleUtils, new RoCCCommand)))
  }
}

class FencePipeConnector(fenceReqName: String, fenceRespName: String, clock: Clock, io: RoCCIO)
               (implicit p: Parameters, cosimTestDetails: CosimTestDetails) extends AbstractCosimPipe {
    val fenceReqPipe = s"${cosimTestDetails.testPath.get}/cosim_run_dir/${fenceReqName}"
    val fenceRespPipe = s"${cosimTestDetails.testPath.get}/cosim_run_dir/${fenceRespName}"

    val monitor = new DecoupledMonitor(clock, io.cmd)
    
    fork {
      while (!Files.exists(Paths.get(fenceReqPipe)) || !Files.exists(Paths.get(fenceRespPipe))) {
        Thread.sleep(250)
      }

      val req = new FileInputStream(fenceReqPipe)

      clock.step()

      while (true) {
        try {
          val r = FenceProtos.FenceReq.parseDelimitedFrom(req)
          if (r != null && r.getValid()) {
            val resp = new FileOutputStream(fenceRespPipe)

            while (io.busy.peek.litToBoolean || monitor.monitoredTransactions.size < r.getNum()) {
              clock.step()
            } // Step clock while busy

            FenceProtos.FenceResp.newBuilder().setComplete(true).build().writeTo(resp)
            resp.close()
          }
        }
        catch {
          case _: IOException => println("IO Exception thrown in Fence Pipe")
        }
        clock.step()
      }
    }
}

case class TLCosimMemoryBufferState(txnBuffer: Seq[TLChannel])

class TLCosimMemoryInterface(tlaPipe: String, tldPipe: String, bundleParams: TLBundleParameters)
                            (implicit p: Parameters, cosimTestDetails: CosimTestDetails) extends TLSlaveFunction[TLCosimMemoryBufferState] {
  while (!Files.exists(Paths.get(tlaPipe)) || !Files.exists(Paths.get(tldPipe))) {
    Thread.sleep(250)
  }

  // NOTE: Scala convention is to open inputs before outputs in matching pairs
  val tld_pipe = new FileInputStream(tldPipe)

  override def response(tx: TLChannel, state: TLCosimMemoryBufferState): (Seq[TLChannel], TLCosimMemoryBufferState) = {
    tx match {
      case txA: TLBundleA =>
        val tla_buffer = state.txnBuffer :+ txA
        if (TLUtils.isCompleteTLTxn(tla_buffer, 16)) {
          tla_buffer.foreach(tla => {
            val tla_proto = VerifProtoBufUtils.BundleToProto(tla, TLProtos.TLA.newBuilder())

            val tla_pipe = new FileOutputStream(tlaPipe)
            tla_proto.writeTo(tla_pipe)
            tla_pipe.close()
          })

          var tld_buffer = Seq[TLChannel]()
          do {
            var tld_proto = com.verif.TLProtos.TLD.parseDelimitedFrom(tld_pipe)
            while(tld_proto == null) {
              tld_proto = com.verif.TLProtos.TLD.parseDelimitedFrom(tld_pipe)
            }
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

class TLPipeConnector(tlaName: String, tldName: String, clock: Clock, io: TLBundle)
            (implicit p: Parameters, cosimTestDetails: CosimTestDetails) extends AbstractCosimPipe {

  val tlaPipe = s"${cosimTestDetails.testPath.get}/cosim_run_dir/${tlaName}"
  val tldPipe = s"${cosimTestDetails.testPath.get}/cosim_run_dir/${tldName}"

  val driver = new TLDriverSlave(clock, io, new TLCosimMemoryInterface(tlaPipe, tldPipe, io.params), TLCosimMemoryBufferState(Seq()))
}
