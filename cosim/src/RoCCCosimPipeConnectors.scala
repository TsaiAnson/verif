package cosim

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chiseltest._
import verif._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.RoCCCommand

class RoCCCommandCosimPipeDriver(pipe: String, clock: Clock, target: verif.DecoupledDriver[RoCCCommand])(implicit p: Parameters) extends
  AbstractCosimPipeDriver[DecoupledIO[RoCCCommand], DecoupledTX[RoCCCommand], RoCCCommand](pipe){

    val driver = target

    val inputStreamToLiteral = (input: java.io.InputStream) => {
      VerifProtoBufUtils.ProtoToBundle(com.verif.RoCCProtos.RoCCCommand.parseDelimitedFrom(input), VerifRoCCUtils, new RoCCCommand)
    }

    def pushIntoDriver(message: RoCCCommand): Unit = {
      driver.push(new DecoupledTX(message))
    }

}