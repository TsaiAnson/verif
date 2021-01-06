package cosim

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chiseltest._
import verif._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.RoCCCommand
import com.verif.RoCCProtos

class RoCCCommandCosimPipeDriver(pipe: String, clock: Clock, target: verif.DecoupledDriver[RoCCCommand])(implicit p: Parameters) extends
  AbstractCosimPipeDriver[DecoupledIO[RoCCCommand], DecoupledTX[RoCCCommand], RoCCProtos.RoCCCommand](pipe){

    val driver = target

    val inputStreamToProto = (input: java.io.InputStream) => {
      com.verif.RoCCProtos.RoCCCommand.parseDelimitedFrom(input)
    }

    def pushIntoDriver(message: RoCCProtos.RoCCCommand): Unit = {
      driver.push(new DecoupledTX(VerifProtoBufUtils.ProtoToBundle(message, VerifRoCCUtils, new RoCCCommand)))
    }

}