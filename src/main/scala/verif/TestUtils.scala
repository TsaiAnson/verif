//******************************************************************************
// Copyright (c) 2018 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package verif

import org.scalatest._

import chisel3._
import chisel3.util._

import chipyard.{RocketConfig}

import freechips.rocketchip.config._
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case object VerifTileParams extends TileParams {
  val name: Option[String] = Some("verif_tile")
  val hartId: Int = 0
  val core: RocketCoreParams = RocketCoreParams()
  val beuAddr: Option[BigInt] = None
  val blockerCtrlAddr: Option[BigInt] = None
  val btb: Option[BTBParams] = None
  val dcache: Option[DCacheParams] = Some(DCacheParams())
  val icache: Option[ICacheParams] = Some(ICacheParams())
}

/**
 * Factory object to help create a set of Verif parameters to use in tests
 */
object VerifTestUtils {
  def getVerifParameters(
      xLen: Int = 64,
      beatBytes: Int = 16,
      blockBytes: Int = 64,
      pAddrBits: Int = 32,
      transferSize: TransferSizes = TransferSizes(1, 64)): Parameters = {

    val origParams = new RocketConfig
    //val origParams = Parameters.empty

    // augment the parameters
    implicit val p = origParams.alterPartial {
      case MonitorsEnabled => false
      case TileKey => VerifTileParams
      case XLen => xLen // Have to use xLen to avoid errors with naming
      case PgLevels => if (xLen == 64) 3 else 2
      case MaxHartIdBits => 1
      case SystemBusKey => SystemBusParams(
        beatBytes = beatBytes,
        blockBytes = blockBytes // Is 64 the right value?
      )
    }

    // TODO: should these be args to the main function as well for ease of use?
    def verifTLUBundleParams: TLBundleParameters = TLBundleParameters(addressBits = 64, dataBits = 64, sourceBits = 1,
      sinkBits = 1, sizeBits = 6,
      echoFields = Seq(), requestFields = Seq(), responseFields = Seq(),
      hasBCE = false)

    val dummyInNode = BundleBridgeSource(() => TLBundle(verifTLUBundleParams))
    val dummyOutNode = BundleBridgeSink[TLBundle]()

    val tlMasterXbar = LazyModule(new TLXbar)
    val visibilityNode = TLEphemeralNode()(ValName("tile_master"))

    visibilityNode :=* tlMasterXbar.node
    tlMasterXbar.node :=
      BundleBridgeToTL(TLClientPortParameters(Seq(TLClientParameters("bundleBridgeToTL")))) :=
      dummyInNode
    //TODO: maybe values here paramterized
    //NOTE: address mask needed to be 0xffffffff so that paddrBits was 32 and not 12 (mask 0xfff)
    dummyOutNode :=
      TLToBundleBridge(TLManagerPortParameters(Seq(TLManagerParameters(address = Seq(AddressSet(0x0, BigInt("1"*pAddrBits, 2))),
        supportsGet = transferSize, supportsPutFull = transferSize)), beatBytes)):=
      visibilityNode

    val outParams = p.alterPartial {
      case TileVisibilityNodeKey => visibilityNode
    }

    //orgParams
    outParams
  }
}
