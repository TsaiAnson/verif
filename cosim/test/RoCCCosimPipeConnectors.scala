package cosim

import verif._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.RoCCCommand

class RoCCCommandCosimPipeDriver(pipe: String, driver: DecoupledDriver[RoCCCommand])(implicit p: Parameters) extends
  AbstractCosimPipeDriver(pipe, driver,
      (input: java.io.InputStream) => com.verif.RoCCProtos.RoCCCommand.parseDelimitedFrom(input),
      (cmd: com.google.protobuf.Message) => VerifProtoBufUtils.ProtoToBundle(cmd, VerifRoCCUtils, new RoCCCommand)) {

}