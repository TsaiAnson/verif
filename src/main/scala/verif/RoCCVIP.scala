package verif

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chiseltest._
import firrtl.FirrtlProtos.Firrtl.Statement.Register
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, SimpleDevice}
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile.{RoCCCommand, RoCCInstruction}
import chisel3.experimental.BundleLiterals._
import chisel3.util._

import scala.collection.mutable
import scala.collection.mutable.Queue

object RoCCVerifUtils {
  def RoCCCommandHelper(inst: RoCCInstruction = new RoCCInstruction, rs1: UInt = 0.U, rs2: UInt = 0.U)
                       (implicit p: Parameters): RoCCCommand = {
    new RoCCCommand().Lit(_.inst -> inst, _.rs1 -> rs1, _.rs2 -> rs2)
  }

  def RoCCInstructionHelper(funct: UInt = 0.U, rs2: UInt = 0.U, rs1: UInt = 0.U, xd: Bool = false.B,
                            xs1: Bool = false.B, xs2: Bool = false.B, rd: UInt = 0.U,
                            opcode: UInt = 0.U): RoCCInstruction = {
    new RoCCInstruction().Lit(_.funct -> funct, _.rs2 -> rs2, _.rs1 -> rs1, _.xd -> xd, _.xs1 -> xs1,
                            _.xs2 -> xs2, _.rd -> rd, _.opcode -> opcode)
  }
}

// RoCCCommandDriver
class RoCCCommandDriver(clock: Clock, interface: DecoupledIO[RoCCCommand]) {

  val txns = Queue[RoCCCommand]()

  def push(c: RoCCCommand): Unit = {
    txns += c
  }

  def drive(i: RoCCInstruction): Unit = {
    interface.bits.inst.funct.poke(i.funct)
    interface.bits.inst.rs2.poke(i.rs2)
    interface.bits.inst.rs1.poke(i.rs1)
    interface.bits.inst.xd.poke(i.xd)
    interface.bits.inst.xs1.poke(i.xs1)
    interface.bits.inst.xs2.poke(i.xs2)
    interface.bits.inst.rd.poke(i.rd)
    interface.bits.inst.opcode.poke(i.opcode)
  }

  def drive(c: RoCCCommand): Unit = {
    while (!interface.ready.peek().litToBoolean) {
      clock.step()
    }

    timescope {
      drive(c.inst)
      interface.bits.rs1.poke(c.rs1)
      interface.bits.rs2.poke(c.rs2)
      interface.valid.poke(true.B)
      clock.step()
    }
  }

  fork {
    while (true) {
      if (!txns.isEmpty) {
        val c = txns.dequeue()
        drive(c)
      } else {
        clock.step()
      }
    }
  }
}
