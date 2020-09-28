package rocketdsptools

import chisel3._

import freechips.rocketchip.subsystem._
import freechips.rocketchip.system._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.util.DontTouch

import testchipip._

import utilities.{System, SystemModule}

import sifive.blocks.devices.gpio._

// ------------------------------------
// BOOM and/or Rocket Top Level Systems
// ------------------------------------

class Top(implicit p: Parameters) extends System
  with HasNoDebug
  with HasPeripherySerial {
  override lazy val module = new TopModule(this)
}

class TopModule[+L <: Top](l: L) extends SystemModule(l)
  with HasNoDebugModuleImp
  with HasPeripherySerialModuleImp
  with DontTouch

//---------------------------------------------------------------------------------------------------------

class TopWithTLUIntPassthrough(implicit p: Parameters) extends Top
  with HasPeripheryTLUIntPassthrough {
  override lazy val module = new TopModule(this)
}

class TopWithTLUIntTestFIR(implicit p: Parameters) extends Top
  with HasPeripheryTLUIntTestFIR {
  override lazy val module = new TopModule(this)
}

//---------------------------------------------------------------------------------------------------------

class TopWithDTM(implicit p: Parameters) extends System
{
  override lazy val module = new TopWithDTMModule(this)
}

class TopWithDTMModule[+L <: TopWithDTM](l: L) extends SystemModule(l)
