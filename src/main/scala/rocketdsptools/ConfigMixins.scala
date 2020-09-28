package rocketdsptools

import chisel3._
import chisel3.util.{log2Up}

import freechips.rocketchip.config.{Field, Parameters, Config}
import freechips.rocketchip.subsystem.{RocketTilesKey, WithRoccExample, WithNMemoryChannels, WithNBigCores, WithRV32}
import freechips.rocketchip.diplomacy.{LazyModule, ValName}
import freechips.rocketchip.devices.tilelink.BootROMParams
import freechips.rocketchip.tile.{XLen, BuildRoCC, TileKey, LazyRoCC}

import boom.common.{BoomTilesKey}

import testchipip._

import hwacha.{Hwacha}

import sifive.blocks.devices.gpio._

object ConfigValName {
  implicit val valName = ValName("TestHarness")
}
import ConfigValName._

// -----------------------
// Common Parameter Mixins
// -----------------------

/**
 * Class to specify where the BootRom file is (from `rebar` top)
 */
class WithBootROM extends Config((site, here, up) => {
  case BootROMParams => BootROMParams(
    contentFileName = s"./bootrom/bootrom.rv${site(XLen)}.img")
})

class WithTLUIntPassthroughTop extends Config((site, here, up) => {
  case BuildTop => (clock: Clock, reset: Bool, p: Parameters) =>
    Module(LazyModule(new TopWithTLUIntPassthrough()(p)).module)
})

class WithTLUIntTestFIRTop extends Config((site, here,up) => {
  case BuildTop => (clock: Clock, reset: Bool, p:Parameters) =>
    Module(LazyModule(new TopWithTLUIntTestFIR()(p)).module)
})
