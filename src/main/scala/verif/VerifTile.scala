package verif

import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{LogicalTreeNode}
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem.{RocketCrossingParams}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.amba.axi4._

case class VerifTileAttachParams(
  tileParams: VerifTileParams,
  crossingParams: RocketCrossingParams
) extends CanAttachTile {
  type TileType = VerifTile
  val lookup = PriorityMuxHartIdFromSeq(Seq(tileParams))
}

case class VerifTileParams(
  name: Option[String] = Some("verif_tile"),
  hartId: Int = 0,
) extends InstantiableTileParams[VerifTile]
{
  val core: RocketCoreParams = RocketCoreParams()
  val beuAddr: Option[BigInt] = None
  val blockerCtrlAddr: Option[BigInt] = None
  val btb: Option[BTBParams] = None
  val dcache: Option[DCacheParams] = None
  val icache: Option[ICacheParams] = None
  def instantiate(crossing: TileCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters): VerifTile = {
    new VerifTile(this, crossing, lookup)
  }
}

class VerifTile private (
  params: VerifTileParams,
  crossing: ClockCrossingType,
  lookup: LookupByHartIdImpl,
  q: Parameters
) extends BaseTile(params, crossing, lookup, q)
  with SinksExternalInterrupts
  with SourcesExternalNotifications {

  def this(params: VerifTileParams, crossing: TileCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters) =
    this(params, crossing.crossingType, lookup, p)

  val intOutwardNode = IntIdentityNode()
  val masterNode: TLOutwardNode = TLIdentityNode() := visibilityNode := TLTempNode()
  val slaveNode = TLIdentityNode()

  val cpuDevice: SimpleDevice = new SimpleDevice("verif", Nil)

  override lazy val module = new VerifTileModuleImp(this)
}

class VerifTileModuleImp(outer: VerifTile) extends BaseTileModuleImp(outer) {

}

class WithNVerifTiles(n: Int = 1, overrideIdOffset: Option[Int] = None) extends Config((site, here, up) => {
  case TilesLocated(InSubsystem) => {
    // Calculate the next available hart ID (since hart ID cannot be duplicated)
    val prev = up(TilesLocated(InSubsystem), site)
    val idOffset = overrideIdOffset.getOrElse(prev.size)
    // Create TileAttachParams for every core to be instantiated
    (0 until n).map { i =>
      VerifTileAttachParams(
        tileParams = VerifTileParams(hartId = i + idOffset),
        crossingParams = RocketCrossingParams()
      )
    } ++ prev
  }
  // Configurate # of bytes in one memory / IO transaction. For RV64, one load/store instruction can transfer 8 bytes at most.
  case SystemBusKey => up(SystemBusKey, site).copy(beatBytes = 8)
  // The # of instruction bits. Use maximum # of bits if your core supports both 32 and 64 bits.
  case XLen => 64
})