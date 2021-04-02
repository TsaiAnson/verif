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

trait AbstractForkedCosimPipe

class ForkedRoCCCommandPipeDriver(pipeName: String, clock: Clock, io: DecoupledIO[RoCCCommand])(implicit p: Parameters, cosimTestDetails: CosimTestDetails) extends AbstractForkedCosimPipe {
  val pipe = s"${cosimTestDetails.testPath.get}/cosim_run_dir/${pipeName}"
  fork.withName("RoCCCommand Pipe Driver") {
    val driver = new DecoupledDriverMaster(clock, io) // TODO: Move outside fork if possible
    while (!Files.exists(Paths.get(pipe))) {
      Thread.sleep(250)
    }

    val in = new FileInputStream(pipe)

    while (true) {
      val message = com.verif.RoCCProtos.RoCCCommand.parseDelimitedFrom(in)
      if (message != null) {
        driver.push(new DecoupledTX(new RoCCCommand).tx(VerifProtoBufUtils.ProtoToBundle(message, VerifBundleUtils, new RoCCCommand)))
      }
      clock.step()
    }
  }
}

class ForkedFencePipeConnector(fenceReqName: String, fenceRespName: String, clock: Clock, io: RoCCIO)
               (implicit p: Parameters, cosimTestDetails: CosimTestDetails) extends AbstractForkedCosimPipe {

    val fenceReqPipe = s"${cosimTestDetails.testPath.get}/cosim_run_dir/${fenceReqName}"
    val fenceRespPipe = s"${cosimTestDetails.testPath.get}/cosim_run_dir/${fenceRespName}"

    fork.withRegion(Monitor).withName("Fence Pipe Connector") {
      val monitor = new DecoupledMonitor(clock, io.cmd)
      // Spin until all needed FIFOs are created
      while (!Files.exists(Paths.get(fenceReqPipe)) || !Files.exists(Paths.get(fenceRespPipe))) {
        Thread.sleep(250)
      }

      val req = new FileInputStream(fenceReqPipe)
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
      req.close()
    }
}

class ForkedTLPipeConnector(tlaName: String, tldName: String, clock: Clock, io: TLBundle)
            (implicit p: Parameters, cosimTestDetails: CosimTestDetails) extends AbstractForkedCosimPipe {

  val tlaPipe = s"${cosimTestDetails.testPath.get}/cosim_run_dir/${tlaName}"
  val tldPipe = s"${cosimTestDetails.testPath.get}/cosim_run_dir/${tldName}"

  while (!Files.exists(Paths.get(tlaPipe)) || !Files.exists(Paths.get(tldPipe))) {
    Thread.sleep(250)
  }

  val driver = new TLDriverSlave(clock, io, new TLCosimMemoryInterface(tlaPipe, tldPipe, io.params), TLCosimMemoryBufferState(Seq()))
}
