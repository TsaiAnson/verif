package cosim

import chisel3._
import chisel3.util._
import chiseltest._
import verif._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.RoCCCommand

class RoCCCommandCosimPipeDriver(pipe: String, clock: Clock, target: verif.DecoupledDriver[RoCCCommand])(implicit p: Parameters) extends
  AbstractCosimPipeDriver[DecoupledIO[RoCCCommand], DecoupledTX[RoCCCommand], RoCCCommand](pipe){

    val io = new DecoupledIO(new RoCCCommand)
    val driver = target
    val monitor = new verif.DecoupledMonitor[RoCCCommand](clock, io)

    val inputStreamToLiteral = (input: java.io.InputStream) => {
      VerifProtoBufUtils.ProtoToBundle(com.verif.RoCCProtos.RoCCCommand.parseDelimitedFrom(input), VerifRoCCUtils, new RoCCCommand)
    }

    def pokeIntoIO(message: RoCCCommand): Unit = {
      timescope {
        io.valid.poke(true.B)
        io.bits.pokePartial(message)
      }
    }

}