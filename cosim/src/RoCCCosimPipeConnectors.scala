package cosim

import verif._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.RoCCCommand

class RoCCCommandCosimPipeDriver(pipe: String, clock: Clock, driver: DecoupledDriver[RoCCCommand])(implicit p: Parameters) extends
  AbstractCosimPipeDriver(pipe, clock, new RoCCCommand){

      val driver = driver
      val inputStreamToLiteral = (input: java.io.InputStream) => {
            VerifProtoBufUtils.ProtoToBundle(com.verif.RoCCProtos.RoCCCommand.parseDelimitedFrom(input), VerifRoCCUtils, new RoCCCommand)
      }

}