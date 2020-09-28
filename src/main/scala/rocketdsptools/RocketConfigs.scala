package rocketdsptools

import chisel3._

import freechips.rocketchip.config.{Config}

class UIntPassthroughRocketConfig extends Config(
  new WithTLUIntPassthroughTop ++     // use top with tilelink-controlled passthrough
  new WithBootROM ++
  new freechips.rocketchip.subsystem.WithInclusiveCache ++
  new freechips.rocketchip.subsystem.WithNBigCores(1) ++
  new freechips.rocketchip.system.BaseConfig
)

class UIntTestFIRRocketConfig extends Config (
  new WithTLUIntTestFIRTop ++ // use top with tilelink-controller FIR
  new WithBootROM ++
  new freechips.rocketchip.subsystem.WithInclusiveCache ++
  new freechips.rocketchip.subsystem.WithNBigCores(1) ++
  new freechips.rocketchip.system.BaseConfig
)
