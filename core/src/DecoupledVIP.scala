package verif

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chiseltest._

case class DecoupledTX[T <: Data](gen: T) extends Bundle {
  val data: T = gen.cloneType
  val waitCycles: UInt = UInt(32.W)
  val postSendCycles: UInt = UInt(32.W)
  val cycleStamp: UInt = UInt(32.W)

  // TODO: rename to driver/monitor TX
  // TODO: split into driver and monitor TXs
  // TODO: how can we check that data: T fits into gen? (e.g. gen = UInt(2.W), data = 16.U shouldn't work)
  def tx(data: T, waitCycles: Int, postSendCycles: Int): DecoupledTX[T] = {
    this.Lit(_.data -> data, _.waitCycles -> waitCycles.U, _.postSendCycles -> postSendCycles.U, _.cycleStamp -> 0.U)
  }
  def tx(data: T): DecoupledTX[T] = {
    this.Lit(_.data -> data, _.waitCycles -> 0.U, _.postSendCycles -> 0.U)
  }
  def tx(data: T, cycleStamp: Int): DecoupledTX[T] = {
    this.Lit(_.data -> data, _.cycleStamp -> cycleStamp.U)
  }
}

// TODO: combine driver and monitor into VIP/Agent to keep API clean
// TODO: VIP/Agent should have master and slave modes (monitor should never peek)
class DecoupledDriver[T <: Data](clock: Clock, interface: DecoupledIO[T]) extends
  AbstractDriver[DecoupledIO[T], DecoupledTX[T]](clock, interface) {

  fork {
    var cycleCount = 0
    var idleCycles = 0
    while (true) {
      if (hasNextTransaction && idleCycles == 0) {
        val t = getNextTransaction
        if (t.waitCycles.litValue().toInt > 0) {
          idleCycles = t.waitCycles.litValue().toInt
          while (idleCycles > 0) {
            // For debugging use
            // println("IDUT", cycleCount)
            idleCycles -= 1
            cycleCount += 1
            clock.step()
          }
        }
        while (!interface.ready.peek().litToBoolean) {
          cycleCount += 1
          clock.step()
        }

        cycleCount += 1
        timescope {
          if (t.data.isInstanceOf[Bundle]) {
            interface.bits.asInstanceOf[Bundle].pokePartial(t.data.asInstanceOf[Bundle])
          } else {
            interface.bits.poke(t.data)
          }
          interface.valid.poke(true.B)
          clock.step()
        }

        idleCycles = t.postSendCycles.litValue().toInt
      } else {
        if (idleCycles > 0) idleCycles -= 1
        cycleCount += 1
        // For debugging use
        // println("IDUT", cycleCount)
        clock.step()
      }
    }
  }
}

class DecoupledMonitor[T <: Data](clock: Clock, interface: DecoupledIO[T], waitCycles: Int) extends
  AbstractMonitor[DecoupledIO[T], DecoupledTX[T]](clock, interface) {
  fork {
    var cycleCount = 0
    var idleCyclesD = 0
    while (true) {
      interface.ready.poke(false.B)
      while (idleCyclesD > 0) {
        idleCyclesD -= 1
        cycleCount += 1
        clock.step()
      }
      interface.ready.poke(1.B)
      if (interface.valid.peek().litToBoolean) {
        val t = DecoupledTX(interface.bits.cloneType.asInstanceOf[T]) // asInstanceOf[T] to make IntelliJ happy
        val tLit = t.Lit(_.data -> interface.bits.peek(), _.cycleStamp -> cycleCount.U)
        idleCyclesD = waitCycles
        addMonitoredTransaction(tLit)
      }
      cycleCount += 1
      clock.step()
    }
  }
}
